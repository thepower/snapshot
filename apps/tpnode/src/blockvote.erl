%%% -------------------------------------------------------------------
%%% "ThePower.io". Copyright (C) 2018 Mikhaylenko Maxim, Belousov Igor
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

-module(blockvote).

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

init(_Args) ->
    self() ! init,
    {ok, undefined}.

handle_call(_, _From, undefined) ->
    {reply, notready, undefined};

handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(settings, State) ->
    {noreply, load_settings(State)};

handle_cast(_, undefined) ->
    {noreply, undefined};

handle_cast({tpic, From, Bin}, State) when is_binary(Bin) ->
    case msgpack:unpack(Bin) of
        {ok, Struct} ->
            handle_cast({tpic, From, Struct}, State);
        _Any ->
            lager:info("Can't decode TPIC ~p", [_Any]),
            {noreply, State}
    end;

handle_cast({tpic, _From, #{
                     null:=<<"blockvote">>,
                     <<"chain">>:=MsgChain,
                     <<"hash">> := BlockHash,
                     %<<"n">> := _OriginNode,
                     <<"sign">> := Sigs
                    }},
            #{mychain:=MyChain}=State) when MyChain==MsgChain ->
    handle_cast({signature, BlockHash, Sigs}, State);

handle_cast({tpic, _From, #{
                     null:=<<"blockvote">>,
                     <<"chain">>:=MsgChain,
                     <<"hash">> := _BlockHash,
                     %<<"n">> := _OriginNode,
                     <<"sign">> := _Sigs
                    }},
            #{mychain:=MyChain}=State) when MyChain=/=MsgChain ->
    lager:info("BV sig from other chain"),
    {noreply, State};

handle_cast({signature, BlockHash, _Sigs}=WholeSig,
            #{lastblock:=#{hash:=LBH}}=State) when LBH==BlockHash->
    lager:info("BV Got extra sig for ~s ~p", [blkid(BlockHash), WholeSig]),
    gen_server:cast(blockchain, WholeSig),
    {noreply, State};


handle_cast({signature, BlockHash, Sigs}, #{candidatesig:=Candidatesig}=State) ->
    lager:info("BV Got sig for ~s", [blkid(BlockHash)]),
    CSig0=maps:get(BlockHash, Candidatesig, #{}),
    CSig=checksig(BlockHash, Sigs, CSig0),
    %lager:debug("BV S CS2 ~p", [maps:keys(CSig)]),
    State2=State#{ candidatesig=>maps:put(BlockHash, CSig, Candidatesig) },
    {noreply, is_block_ready(BlockHash, State2)};

handle_cast({new_block, #{hash:=BlockHash, sign:=Sigs}=Blk, _PID},
            #{ candidates:=Candidates,
               candidatesig:=Candidatesig,
               lastblock:=#{hash:=LBlockHash}=LastBlock
             }=State) ->

    lager:info("BV New block (~p/~p) arrived (~s/~s)",
               [
                maps:get(height, maps:get(header, Blk)),
                maps:get(height, maps:get(header, LastBlock)),
                blkid(BlockHash),
                blkid(LBlockHash)
               ]),
    CSig0=maps:get(BlockHash, Candidatesig, #{}),
    CSig=checksig(BlockHash, Sigs, CSig0),
    %lager:debug("BV N CS2 ~p", [maps:keys(CSig)]),
    State2=State#{ candidatesig=>maps:put(BlockHash, CSig, Candidatesig),
                   candidates => maps:put(BlockHash, Blk, Candidates)
                 },
    {noreply, is_block_ready(BlockHash, State2)};

handle_cast(blockchain_sync, State) ->
    LastBlock=gen_server:call(blockchain, last_block),
    Res=State#{
      lastblock=>LastBlock
     },
    {noreply, Res};

handle_cast(_Msg, State) ->
    lager:info("BV Unknown cast ~p", [_Msg]),
    {noreply, State}.

handle_info(init, undefined) ->
    LastBlock=gen_server:call(blockchain, last_block),
    lager:info("BV My last block hash ~s",
               [bin2hex:dbin2hex(maps:get(hash, LastBlock))]),
    Res=#{
      candidatesig=>#{},
      candidates=>#{},
      lastblock=>LastBlock
     },
    {noreply, load_settings(Res)};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    lager:error("Terminate blockvote ~p", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

blkid(<<X:8/binary, _/binary>>) ->
    bin2hex:dbin2hex(X).

checksig(BlockHash, Sigs, Acc0) ->
    lists:foldl(
      fun(Signature, Acc) ->
              case bsig:checksig1(BlockHash, Signature) of
                  {true, #{extra:=Xtra}=US} ->
                      Pub=proplists:get_value(pubkey, Xtra),
                      lager:debug("BV ~s Check sig ~s", [
                                      blkid(BlockHash),
                                      bin2hex:dbin2hex(Pub)
                                     ]),
					  case maps:is_key(Pub, Acc) of
						  true -> Acc;
						  false ->
							  maps:put(Pub, US, Acc)
					  end;
                  false ->
                      Acc
              end
      end, Acc0, Sigs).

is_block_ready(BlockHash, State) ->
	try
		MinSig=maps:get(minsig, State, 2),
		T0=erlang:system_time(),
		Sigs=try
				 maps:get(BlockHash, maps:get(candidatesig, State))
			 catch _:_ ->
					   throw({notready, nosig})
			 end,
		case maps:is_key(BlockHash, maps:get(candidates, State)) of
			false ->
				case maps:size(Sigs) >= MinSig of
					true ->
						%throw({notready, nocand1}),
						lager:info("Probably they went ahead"),
						blockchain ! checksync,
						State;
					false ->
						throw({notready, {nocand, maps:size(Sigs), MinSig}})
				end;
			true ->
				Blk0=maps:get(BlockHash, maps:get(candidates, State)),
				Blk1=Blk0#{sign=>maps:values(Sigs)},
				{true, {Success, _}}=block:verify(Blk1),
				T1=erlang:system_time(),
				Txs=maps:get(txs, Blk0, []),
				lager:notice("TODO: Check keys ~p of ~p",
							 [length(Success), MinSig]),
				if length(Success)<MinSig ->
					   lager:info("BV New block ~w arrived ~s, txs ~b, verify ~w (~.3f ms)",
								  [maps:get(height, maps:get(header, Blk0)),
								   blkid(BlockHash),
								   length(Txs),
								   length(Success),
								   (T1-T0)/1000000]),
					   throw({notready, minsig});
				   true ->
					   lager:info("BV New block ~w arrived ~s, txs ~b, verify ~w (~.3f ms)",
								  [maps:get(height, maps:get(header, Blk0)),
								   blkid(BlockHash),
								   length(Txs),
								   length(Success),
								   (T1-T0)/1000000])
				end,
				Blk=Blk0#{sign=>Success},
				%enough signs. use block
				T3=erlang:system_time(),
				lager:info("BV enough confirmations. Installing new block ~s h= ~b (~.3f ms)",
						   [blkid(BlockHash),
							maps:get(height, maps:get(header, Blk)),
							(T3-T0)/1000000
						   ]),

				gen_server:cast(blockchain, {new_block, Blk, self()}),
				State#{
				  lastblock=> Blk,
				  candidates=>#{},
				  candidatesig=>#{}
				 }
		end
	catch throw:{notready, Where} ->
			  lager:info("Not ready ~s ~p", [blkid(BlockHash), Where]),
			  State;
		  Ec:Ee ->
			  S=erlang:get_stacktrace(),
			  lager:error("BV New_block error ~p:~p", [Ec, Ee]),
			  lists:foreach(
				fun(Se) ->
						lager:error("at ~p", [Se])
				end, S),
			  State
	end.

load_settings(State) ->
    MyChain=blockchain:get_mysettings(chain),
    MinSig=chainsettings:get_val(minsig,1000),
	LastBlock=gen_server:call(blockchain, last_block),
    lager:info("BV My last block hash ~s",
               [bin2hex:dbin2hex(maps:get(hash, LastBlock))]),
    State#{
      mychain=>MyChain,
      minsig=>MinSig,
      lastblock=>LastBlock
     }.


