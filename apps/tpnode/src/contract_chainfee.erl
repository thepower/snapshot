%%% -------------------------------------------------------------------
%%% "ThePower.io". Copyright (C) 2018 Mihaylenko Maxim, Belousov Igor
%%%
%%% This program is not free software; you can not redistribute it and/or modify it
%%% in accordance the following terms and conditions.
%%% -------------------------------------------------------------------
%%%
%%% TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%%
%%% 0. This License applies to any program or other work which contains a notice
%%% placed by the copyright holder saying it may be distributed under the terms of
%%% this License. The "Program", below, refers to any such program or work.Each
%%% licensee is addressed as "you".
%%%
%%% 1. You can use this Program only in case of personal non-commercial use.
%%%
%%% 2. You may not copy and distribute copies of the Program and Program's source
%%% code as you receive it.
%%%
%%% 3. You may not modify your copy or copies of the Program or any portion of it,
%%% thus forming a work based on the Program, and copy and distribute such
%%% modifications or work.
%%%
%%% 4. You may not copy, modify, sublicense, or distribute the Program in object
%%% code or executable form. Any attempt to copy, modify, sublicense or distribute
%%% the Program is void, and will automatically terminate your rights under this
%%% License.
%%%
%%% NO WARRANTY
%%%
%%% 5. THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE
%%% LAW. PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED
%%% OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE
%%% QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE
%%% DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
%%%
%%% 6. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL
%%% ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE
%%% PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL,
%%% SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY
%%% TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
%%% RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF
%%% THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER
%%% PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
%%%
%%% END OF TERMS AND CONDITIONS

-module(contract_chainfee).
-behaviour(smartcontract).

-export([deploy/6, handle_tx/4, getters/0, get/3, info/0]).

info() ->
	{<<"chainfee">>, <<"distibute collected fee over nodes">>}.

deploy(_Address, _Ledger, Code, _State, _GasLimit, GetFun) ->
	Settings=#{interval:=Interval}=erlang:binary_to_term(Code, [safe]),
	#{header:=#{height:=Height}}=GetFun({get_block, 0}),
	%lager:info("B0 ~p", [Height]),
	lager:info("Deploying chainfee contract ~p",
			  [Settings]),
	{ok, term_to_binary(#{
						  last_h=>Height-12,
						  interval=>Interval
						 })}.

handle_tx(#{to:=MyAddr}=_Tx, Ledger, _GasLimit, GetFun) ->
	MyState=bal:get(state, Ledger),
	#{interval:=Int, last_h:=LH}=State=erlang:binary_to_term(MyState, [safe]),
	#{header:=#{height:=CurHeight}}=GetFun({get_block, 0}),
	lager:info("chainfee ~p ~w", [State, CurHeight-(LH+Int)]),
	if CurHeight>=LH+Int ->
		   Collected=maps:get(amount, Ledger),
		   LS=max(bal:get(seq, Ledger), maps:get(seq, State, 0)),
		   Nodes=lists:foldl(
			 fun(N, Acc) ->
					 Blk=GetFun({get_block, N}),
					 ED=maps:get(extdata, Blk, []),
					 F=fun(Found) ->
							   if Acc==undefined ->
									  lists:foldl(
										fun(E, A) ->
												maps:put(E, 1, A)
										end, #{}, Found);
								  true ->
									  lists:foldl(
										fun(E, A) ->
												maps:put(E, 1+maps:get(E, A, 0), A)
										end, Acc, Found)
							   end
					   end,
					 case proplists:get_value(<<"prevnodes">>, ED) of
						 undefined ->
							 case proplists:get_value(prevnodes, ED) of
								 undefined ->
									 Acc;
								 Found ->
									 F(Found)
							 end;
						 Found ->
							 F(Found)
					 end
			 end, undefined, lists:seq(0, Int-1)),
		   MaxN=maps:fold(
				  fun(_, V, A) when V>A -> V;
					 (_, _, A) -> A
				  end, 0, Nodes),
		   Worthy=maps:fold(
					fun(K, V, A) ->
							if(V==MaxN)->
								  Wallet=settings:get(
										   [<<"current">>, <<"rewards">>, K],
										   GetFun(settings)),
								  if is_binary(Wallet) ->
										 [Wallet|A];
									 true -> A
								  end;
							  true ->
								  A
							end
					end, [], Nodes),
		   lager:info("collected ~p for ~p", [Collected, Worthy]),
		   WL=length(Worthy),
		   if WL>0 ->
				  {NewLS, TXs}=maps:fold(
								fun(Token, Amount, Acc) ->
										Each=trunc(Amount/WL),
										lists:foldl(
										  fun(Wallet, {SI, Acc1}) ->
												  {SI+1,
												   [#{
													 from=>MyAddr,
													 to=>Wallet,
													 cur=>Token,
													 amount=>Each,
													 seq=>SI+1,
													 timestamp=>0
													}|Acc1]}
										  end, Acc, Worthy)
								end, {LS, []}, Collected),
				  TXs1=lists:reverse(TXs),
				  {ok, erlang:term_to_binary(
						 State#{
						   last_h=>CurHeight,
						   seq=>NewLS
						  }), 0, TXs1};
			  true ->
				  {ok, erlang:term_to_binary(
						 State#{
						   last_h=>CurHeight,
						   seq=>LS
						  }), 0, []}
		   end;
	   true ->
		   {ok, unchanged, 0}
	end.

getters() ->
	[{<<"collected">>,[]},
	 {<<"sum">>,[{<<"Первое слагаемое"/utf8>>,int},{<<"Второе слагаемое"/utf8>>,int}]}
	].

get(<<"sum">>,[A,B],_Ledger) ->
	A+B;

get(<<"collected">>,[],Ledger) ->
	MyState=bal:get(state, Ledger),
	lager:info("Called ~p",[maps:keys(Ledger)]),
	#{last_h:=LH}=erlang:binary_to_term(MyState, [safe]),
	Collected=maps:get(amount, Ledger),
	#{last_height=>LH,
		collected=>Collected};

get(_,_,_Ledger) ->
	throw("unknown method").


