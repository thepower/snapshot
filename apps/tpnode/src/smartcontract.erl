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

-module(smartcontract).
-export([run/5,info/1,getters/1,get/4]).

-callback deploy(Address :: binary(),
				 Ledger :: map(),
				 Code :: binary(),
				 State :: binary()|undefined,
				 GasLimit :: integer(),
				 GetFun :: fun()) ->
	{'ok', NewLedger :: map()}.

-callback handle_tx(Tx :: map(),
					Ledger :: map(),
					GasLimit :: integer(),
					GetFun :: fun()) ->
	{'ok',  %success finish, emit new txs
	 NewState :: 'unchanged' | binary(), % atom unchanged if no state changed
	 GasUsed :: integer(),
	 EmitTxs :: list()
	} |
	{'ok',  %success finish
	 NewState :: 'unchanged' | binary(), % atom unchanged if no state changed
	 GasUsed :: integer()
	} |
	{'error', %error during execution
	 Reason :: 'insufficient_gas' | string(),
	 GasUsed :: integer()
	} |
	{'error', %error during start
	 Reason :: string()
	}.

-callback info() -> {Name::binary(), Descr::binary()}.
-type args() :: [{Arg::binary(),int|bin|addr}].
-type fa() :: {Method::binary(), Args::args()}|{Method::binary(), Args::args(), Descr::binary()}.
-callback getters() -> [Getter::fa()].
-callback get(Method::binary(), Args::[binary()|integer()], Ledger :: map()) -> [Getter::mfa()].

info(VMType) ->
	try
		A=erlang:binary_to_existing_atom(<<"contract_", VMType/binary>>, utf8),
		{CN,CD}=erlang:apply(A, info, []),
		{ok,CN,CD}
	catch error:badarg ->
					error
	end.

get(VMType,Method,Args,Ledger) -> 
	try
		A=erlang:binary_to_existing_atom(<<"contract_", VMType/binary>>, utf8),
		Getters=erlang:apply(A, getters, []),
		case proplists:get_value(Method, Getters) of
			L when is_list(L) ->
				if(length(Args) =/= length(L)) -> 
						throw("bad_args_count");
					true ->
						Args1=lists:map(
										fun({{_,int},Value}) ->
												binary_to_integer(Value);
											 ({{_,addr},Value}) ->
												naddress:parse(Value);
											 ({{_,bin},Value}) ->
												Value;
											 ({{_,Unknown},_}) ->
												throw({"unsupported_type",Unknown})
										end,
										lists:zip(L,Args)),
						lager:info("Expected args ~p",[lists:zip(L,Args)]),
						lager:info("Calling ~p:get(~p,~p,~p)",[A,Method,Args1,Ledger]),
						Res=erlang:apply(A, get, [Method,Args1,Ledger]),
						{ok,Res}
				end;
			undefined ->
				throw("bad_method")
		end
	catch error:badarg ->
					error
	end.

getters(VMType) -> 
	try
		A=erlang:binary_to_existing_atom(<<"contract_", VMType/binary>>, utf8),
		Res=erlang:apply(A, getters, []),
		{ok,Res}
	catch error:badarg ->
					error
	end.

run(VMType, #{to:=To}=Tx, Ledger, GasLimit, GetFun) ->
	VM=try
		   erlang:binary_to_existing_atom(<<"contract_", VMType/binary>>, utf8)
	   catch error:badarg ->
				 throw('unknown_vm')
	   end,
	lager:info("run contract ~s for ~s", [VM, naddress:encode(To)]),
	try
		case erlang:apply(VM,
						  handle_tx,
						  [Tx, Ledger, GasLimit, GetFun]) of
			{ok, NewState, GasUsed, EmitTxs} when
				  NewState==unchanged orelse is_binary(NewState) ->
				if NewState == unchanged ->
					   {Ledger, EmitTxs, GasUsed};
				   true ->
					   {
						bal:put(state, NewState, Ledger),
						EmitTxs, GasUsed}
				end;
			{ok, NewState, GasUsed} when
				  NewState==unchanged orelse is_binary(NewState) ->
				if NewState == unchanged ->
					   {Ledger, [], GasUsed};
				   true ->
					   {
						bal:put(state, NewState, Ledger),
						[], GasUsed}
				end;
			{error, Reason, GasUsed} ->
				%throw({'run_failed', Reason});
				lager:error("Contract error ~p", [Reason]),
				{Ledger, [], GasUsed};
			{error, Reason} ->
				throw({'run_failed', Reason});
			Any ->
				lager:error("Contract return error ~p", [Any]),
				throw({'run_failed', other})
		end
	catch Ec:Ee ->
			  S=erlang:get_stacktrace(),
			  lager:error("Can't run contract ~p:~p @ ~p",
						  [Ec, Ee, hd(S)]),
			  throw({'contract_error', [Ec, Ee]})
	end.

