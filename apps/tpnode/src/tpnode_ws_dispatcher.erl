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

-module(tpnode_ws_dispatcher).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    {ok, #{
       blocksub=>[],
       addrsub=>#{},
       pidsub=>#{},
	   txsub=>[]
      }
    }.

handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(repeat, #{addrsub:=AS, blocksub:=BS}=State) ->
    {ok, [Block]}=file:consult("lastblock.txt"),
    PrettyBlock=tpnode_httpapi:prettify_block(Block),
    maps:fold(
      fun(Address, BalSnap, _) ->
              {Tx, Bal}=lists:foldl(
                         fun({Pid, Acts}, {ATx, ABal}) ->
                                 {
                                  case lists:member(tx, Acts) of
                                      true -> [Pid|ATx];
                                      false -> ATx
                                  end,
                                  case lists:member(bal, Acts) of
                                      true -> [Pid|ABal];
                                      false -> ABal
                                  end
                                 }
                         end, {[], []}, maps:get(Address, AS, [])),
              if Tx=/=[] ->
                     lager:info("Notify TX ~p", [Tx]),
                     BTxs=lists:filter(
                            fun({_TxID, #{from:=Fa}}) when Fa==Address -> true;
                               ({_TxID, #{to:=Ta}}) when Ta==Address -> true;
                               (_) -> false
                            end,
                            maps:get(txs, PrettyBlock)),
                     TxJS=jsx:encode(#{txs=>BTxs,
                                       address=>Address}),
                     lists:foreach(fun(Pid) ->
                                           erlang:send(Pid, {message, TxJS})
                                   end, BS);
                 true ->
                     ok
              end,

              if Bal=/=[] ->
                     lager:info("Notify Bal ~p", [Bal]),
                     BalJS=jsx:encode(#{balance=>BalSnap,
                                        address=>Address}),
                     lists:foreach(fun(Pid) ->
                                           erlang:send(Pid, {message, BalJS})
                                   end, BS);
                 true ->
                     ok
              end,
              ok
      end, undefined, maps:get(bals, PrettyBlock)),
    BlockJS=jsx:encode(#{block=>PrettyBlock}),
    lists:foreach(fun(Pid) ->
                          erlang:send(Pid, {message, BlockJS})
                  end, BS),
    {noreply, State};

handle_cast({new_block, Block}, #{addrsub:=AS, blocksub:=BS}=State) ->
    PrettyBlock=try
                    tpnode_httpapi:prettify_block(Block)
                catch _:_ ->
                          #{error => true}
                end,
    maps:fold(
      fun(Address, BalSnap, _) ->
              {Tx, Bal}=lists:foldl(
                         fun({Pid, Acts}, {ATx, ABal}) ->
                                 {
                                  case lists:member(tx, Acts) of
                                      true -> [Pid|ATx];
                                      false -> ATx
                                  end,
                                  case lists:member(bal, Acts) of
                                      true -> [Pid|ABal];
                                      false -> ABal
                                  end
                                 }
                         end, {[], []}, maps:get(Address, AS, [])),
              if Tx=/=[] ->
                     lager:info("Notify TX ~p", [Tx]),
                     BTxs=lists:filter(
                            fun({_TxID, #{from:=Fa}}) when Fa==Address -> true;
                               ({_TxID, #{to:=Ta}}) when Ta==Address -> true;
                               (_) -> false
                            end,
                            maps:get(txs, PrettyBlock)),
                     TxJS=jsx:encode(#{txs=>BTxs,
                                       address=>Address}),
                     lists:foreach(fun(Pid) ->
                                           erlang:send(Pid, {message, TxJS})
                                   end, Tx);
                 true ->
                     ok
              end,

              if Bal=/=[] ->
                     lager:info("Notify Bal ~p", [Bal]),
                     BalJS=jsx:encode(#{balance=>BalSnap,
                                        address=>Address}),
                     lists:foreach(fun(Pid) ->
                                           erlang:send(Pid, {message, BalJS})
                                   end, Bal);
                 true ->
                     ok
              end,
              ok
      end, undefined, maps:get(bals, PrettyBlock)),
    BlockJS=jsx:encode(#{block=>PrettyBlock}),
    lists:foreach(fun(Pid) ->
                          erlang:send(Pid, {message, BlockJS})
                  end, BS),
    case length(maps:get(txs, Block)) of
        0 ->
            ok;
        _ ->
            file:write_file("tmp/lastblock_ws.txt",
                            iolist_to_binary(
                              io_lib:format("~p.~n", [Block])
                             )
                           )
    end,

    {noreply, State};

handle_cast({done, Result, Txs}, State) ->
	BS=maps:get(txsub, State, []),
	lists:foreach(
	  fun({TxID, Reason}) ->
			  BlockJS=jsx:encode(#{txid=>TxID,
								   result=>Result,
								   info=>format_reason(Reason)
								  }),
			  lists:foreach(fun(Pid) ->
									erlang:send(Pid, {message, BlockJS})
							end, BS);
		 (TxID) ->
			  BlockJS=jsx:encode(#{txid=>TxID,
								   result=>Result
								  }),
			  lists:foreach(fun(Pid) ->
									erlang:send(Pid, {message, BlockJS})
							end, BS)
	  end, Txs),
	{noreply, State};

handle_cast({subscribe, tx, Pid}, #{pidsub:=PS}=State) ->
	TS=maps:get(txsub, State, []),
    monitor(process, Pid),
    {noreply, State#{
                txsub=>[Pid|TS],
                pidsub=>maps:put(Pid, [tx|maps:get(Pid, PS, [])], PS)
               }
    };

handle_cast({subscribe, block, Pid}, #{blocksub:=BS, pidsub:=PS}=State) ->
    monitor(process, Pid),
    {noreply, State#{
                blocksub=>[Pid|BS],
                pidsub=>maps:put(Pid, [block|maps:get(Pid, PS, [])], PS)
               }
    };

handle_cast({subscribe, address, Address, Subs, Pid}, #{addrsub:=AS, pidsub:=PS}=State) ->
    monitor(process, Pid),
    {noreply, State#{
                addrsub=>maps:put(Address, [{Pid, Subs}|maps:get(Address, AS, [])], AS),
                pidsub=>maps:put(Pid, [{addr, Address}|maps:get(Pid, PS, [])], PS)
               }
    }.

handle_info({'DOWN', _Ref, process, Pid, _Reason},
			#{addrsub:=AS0, blocksub:=BS0, pidsub:=PS}=State) ->
	TS0=maps:get(txsub, State, []),
	Subs=maps:get(Pid, PS, []),
	{BS1, TS1, AS1}=lists:foldl(
					fun(block, {BS, TS, AS}) ->
							{lists:delete(Pid, BS), TS, AS};
					   (tx, {BS, TS, AS}) ->
							{BS, lists:delete(Pid, TS), AS};
					   ({addr, A}, {BS, TS, AS}) ->
							AAS=lists:filter(
								  fun({PP, _}) -> PP=/=Pid
								  end, maps:get(A, AS, [])),
							if AAS == [] ->
								   {BS, TS, maps:remove(A, AS)};
							   true ->
								   {BS, TS, maps:put(A, AAS, AS)}
							end
					end, {BS0, TS0, AS0}, Subs),
	{noreply, State#{
				blocksub=>BS1,
				addrsub=>AS1,
				txsub=>TS1,
				pidsub=>maps:remove(Pid, PS)
			   }
	};


handle_info(_Info, State) ->
	lager:info("Unknown INFO ~p", [_Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

format_reason(#{address:=Addr}=Reason) when is_binary(Addr) ->
	Reason#{address=>naddress:encode(Addr)};
format_reason(Reason) when is_map(Reason) -> Reason;
format_reason(Reason) when is_atom(Reason) -> Reason;
format_reason(Reason) -> iolist_to_binary( io_lib:format("~p", [Reason])).

