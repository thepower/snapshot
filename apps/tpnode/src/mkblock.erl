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

-module(mkblock).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%-compile(nowarn_export_all).
%-compile(export_all).
-endif.

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
    {ok, #{
       nodeid=>nodekey:node_id(),
       preptxl=>[],
       pre=>#{h=>0, tmp=>0},
       settings=>#{}
      }
    }.

handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({tpic, From, Bin}, State) when is_binary(Bin) ->
    case msgpack:unpack(Bin) of
        {ok, Struct} ->
            handle_cast({tpic, From, Struct}, State);
        _Any ->
            lager:info("Can't decode TPIC ~p", [_Any]),
            {noreply, State}
    end;

handle_cast({tpic, FromKey, #{
                     null:=<<"lag">>,
                     <<"lbh">>:=LBH
                    }}, State)  ->
  Origin=chainsettings:is_our_node(FromKey),
  lager:debug("MB Node ~s tells I lagging, his h=~w", [Origin, LBH]),
  {noreply, State};


handle_cast({tpic, FromKey, #{
                     null:=<<"mkblock">>,
           <<"hash">> := ParentHash,
           <<"signed">> := SignedBy
                    }}, State)  ->
  Origin=chainsettings:is_our_node(FromKey),
  lager:debug("MB presig got ~s ~p", [Origin, SignedBy]),
  if Origin==false ->
       {noreply, State};
     true ->
       PreSig=maps:get(presig, State, #{}),
       {noreply,
      State#{
        presig=>maps:put(Origin, {ParentHash, SignedBy}, PreSig)
       }}
  end;

handle_cast({tpic, Origin, #{
                     null:=<<"mkblock">>,
                     <<"chain">>:=_MsgChain,
                     <<"txs">>:=TPICTXs
                    }=Msg}, State)  ->
  TXs=decode_tpic_txs(TPICTXs),
  if TXs==[] -> ok;
     true ->
       lager:info("Got txs from ~s: ~p",
            [
             chainsettings:is_our_node(Origin),
             TXs
            ])
  end,
  handle_cast({prepare, Origin, TXs, maps:get(<<"lbh">>,Msg,undefined)}, State);

handle_cast({prepare, Node, Txs, MH}, #{preptxl:=PreTXL}=State) ->
  Origin=chainsettings:is_our_node(Node),
  if Origin==false ->
       lager:error("Got txs from bad node ~s",
             [bin2hex:dbin2hex(Node)]),
       {noreply, State};
     true ->
       if Txs==[] -> ok;
        true ->
          lager:info("TXs from node ~s: ~p",
               [ Origin, length(Txs) ])
       end,
       MarkTx =
         fun({TxID, TxB0}) ->
      
           % get transaction body from storage
           TxB =
             try
               case TxB0 of
                 {TxID, null} ->
                   case txstorage:get_tx(TxID) of
                     {ok, {TxID, TxBody, _Nodes}} ->
                       {TxID, TxBody}; % got tx body from txstorage
                     _ ->
                       {TxID, null} % error
                   end;
                 _OtherTx ->
                   _OtherTx  % transaction with body or invalid transaction
               end
             catch _Ec0:_Ee0 ->
               utils:print_error("Error", _Ec0, _Ee0, erlang:get_stacktrace()),
               TxB0
             end,
      
           TxB1 =
             try
               case TxB of
                 #{patch:=_} ->
                   VerFun =
                     fun(PubKey) ->
                       NodeID = chainsettings:is_our_node(PubKey),
                       is_binary(NodeID)
                     end,
                   {ok, Tx1} = settings:verify(TxB, VerFun),
                   tx:set_ext(origin, Origin, Tx1);

                 #{hash:=_,
                   header:=_,
                   sign:=_} ->

                   %do nothing with inbound block
                   TxB;

                 _ ->
                   {ok, Tx1} = tx:verify(TxB, [{maxsize, txpool:get_max_tx_size()}]),
                   tx:set_ext(origin, Origin, Tx1)
               end
             catch
               throw:no_transaction ->
                 null;
               _Ec:_Ee ->
                 utils:print_error("Error", _Ec, _Ee, erlang:get_stacktrace()),
                 file:write_file(
                   "tmp/mkblk_badsig_" ++ binary_to_list(nodekey:node_id()),
                   io_lib:format("~p.~n", [TxB])
                 ),
                 TxB
             end,
           case TxB1 of
             null ->
               false;
             _ ->
               {true, {TxID, TxB1}}
           end
         end,

       #{header:=#{height:=CHei}}=blockchain:last_meta(),

       lager:info("Node ~p h=~p, my h=~p", [Origin, MH, CHei]),
       if CHei == undefined orelse MH == CHei orelse MH == undefined orelse true ->
         {noreply,
           State#{
             preptxl=>PreTXL ++ lists:filtermap(MarkTx, Txs)
           }
         };
         CHei > MH ->
           tpic:cast(tpic, <<"mkblock">>,
             msgpack:pack(
               #{null=> <<"lag">>,
                 lbh=>CHei
               }
             )),
           lager:notice("Node ~p lagging, my h=~p his=~p",
             [Origin, CHei, MH]),
           {noreply, State};
         CHei < MH ->
           lager:notice("Am I lagging? I got h=~p, but my h=~p from ~p",
             [MH, CHei, Origin]),
           blockchain ! checksync,
           {noreply, State}
       end
  end;

handle_cast(settings, State) ->
    {noreply, load_settings(State)};

handle_cast(_Msg, State) ->
    lager:info("MB unknown cast ~p", [_Msg]),
    {noreply, State}.

handle_info(process,
  #{settings:=#{mychain:=MyChain, nodename:=NodeName}=MySet, preptxl:=PreTXL0}=State) ->
  
  PreTXL1=lists:foldl(
            fun({TxID, TXB}, Acc) ->
                case maps:is_key(TxID, Acc) of
                  true ->
                    TXB1=tx:mergesig(TXB,
                                     maps:get(TxID, Acc)),
                    {ok, Tx1} = tx:verify(TXB1, [ {maxsize, txpool:get_max_tx_size()} ]),
                    maps:put(TxID, Tx1, Acc);
                  false ->
                    maps:put(TxID, TXB, Acc)
                end
            end, #{}, PreTXL0),
  PreTXL=lists:keysort(1, maps:to_list(PreTXL1)),
  
  stout:log(mkblock_process, [ {node, nodekey:node_name()} ]),
  
  AE=maps:get(ae, MySet, 0),
  B=blockchain:last_meta(),
  PTmp=maps:get(temporary,B,false),

  {PHeight, PHash}=case PTmp of false ->
                                  lager:info("Prev block is permanent, make child"),
                                  #{header:=#{height:=Last_Height1}, hash:=Last_Hash1}=B,
                                  {Last_Height1, Last_Hash1};
                                X when is_integer(X) ->
                                  lager:info("Prev block is temporary, make replacement"),
                                  #{header:=#{height:=Last_Height1, parent:=Last_Hash1}}=B,
                                  {Last_Height1-1, Last_Hash1}
                   end,

  lager:info("-------[MAKE BLOCK h=~w tmp=~w]-------",[PHeight, PTmp]),
  lager:info("Pre ~p",[PreTXL0]),

  PreNodes=try
             PreSig=maps:get(presig, State, #{}),
             BK=maps:fold(
                  fun(_, {BH, _}, Acc) when BH =/= PHash ->
                      Acc;
                     (Node1, {_BH, Nodes2}, Acc) ->
                      [{Node1, Nodes2}|Acc]
                  end, [], PreSig),
             lists:sort(bron_kerbosch:max_clique(BK))
           catch
             Ec:Ee ->
               utils:print_error("Can't calc xsig", Ec, Ee, erlang:get_stacktrace()),
               []
           end,

  try
    T1=erlang:system_time(),
    lager:debug("MB pre nodes ~p", [PreNodes]),

    PropsFun=fun(mychain) ->
                 MyChain;
                (settings) ->
                 chainsettings:by_path([]);
                ({valid_timestamp, TS}) ->
                 abs(os:system_time(millisecond)-TS)<3600000;
                ({endless, From, Cur}) ->
                 EndlessPath=[<<"current">>, <<"endless">>, From, Cur],
                 chainsettings:by_path(EndlessPath)==true;
                ({get_block, Back}) when 32>=Back ->
                 FindBlock=fun FB(H, N) ->
                 case gen_server:call(blockchain, {get_block, H}) of
                   undefined ->
                     undefined;
                   #{header:=#{parent:=P}}=Blk ->
                     if N==0 ->
                          maps:without([bals, txs], Blk);
                        true ->
                          FB(P, N-1)
                     end
                 end
             end,
    FindBlock(last, Back)
  end,
  AddrFun=fun({Addr, _Cur}) ->
              case ledger:get(Addr) of
                #{amount:=_}=Bal -> maps:without([changes],Bal);
                not_found -> bal:new()
              end;
             (Addr) ->
              case ledger:get(Addr) of
                #{amount:=_}=Bal -> maps:without([changes],Bal);
                not_found -> bal:new()
              end
          end,

  NoTMP=maps:get(notmp, MySet, 0),

  Temporary = if AE==0 andalso PreTXL==[] ->
                   if(NoTMP=/=0) -> throw(empty);
                     true ->
                       if is_integer(PTmp) ->
                            PTmp+1;
                          true ->
                            1
                       end
                   end;
                 true ->
                   false
              end,
  #{block:=Block,
    failed:=Failed,
    emit:=EmitTXs}=generate_block:generate_block(PreTXL, {PHeight, PHash}, PropsFun, AddrFun,
                                                 [
                                                  {<<"prevnodes">>, PreNodes}
                                                 ],
                                                 [
                                                  {temporary, Temporary}
                                                 ]
                                                ),
  T2=erlang:system_time(),

  case application:get_env(tpnode,mkblock_debug) of
    undefined ->
      ok;
    {ok, true} ->
      stout:log(mkblock_debug,
               [
                {node_name,NodeName},
                {height, PHeight},
                {phash, PHash},
                {pretxl, PreTXL},
                {fail, Failed},
                {block, Block},
                {temporary, Temporary}
               ]);
      %file:write_file(
      %  "log/"++ utils:make_list(NodeName) ++ "_block_" ++
      %      integer_to_list(PHeight) ++ "_" ++ integer_to_list(T2),
      %  io_lib:format("~p~n~p.~n ~p.~n ~p.~n~n", [PHash, PreTXL, Failed, Block])
      %);
    Any ->
      lager:notice("What does mkblock_debug=~p means?",[Any])
  end,
  Timestamp=os:system_time(millisecond),
  ED=[
      {timestamp, Timestamp},
      {createduration, T2-T1}
     ],
  SignedBlock=sign(Block, ED),
  #{header:=#{height:=NewH}}=Block,
  %cast whole block to my local blockvote
  stout:log(mkblock_done,
            [
             {node_name,NodeName},
             {height, PHeight},
             {block_hdr, maps:with([hash,header,sign,temporary], SignedBlock)}
            ]),

  gen_server:cast(blockvote, {new_block, SignedBlock, self()}),

  case application:get_env(tpnode, dumpblocks) of
    {ok, true} ->
      file:write_file("tmp/mkblk_" ++
                      integer_to_list(NewH) ++ "_" ++
                      binary_to_list(nodekey:node_id()),
                      io_lib:format("~p.~n", [SignedBlock])
                     );
    _ -> ok
  end,
  %Block signature for each other
  lager:debug("MB My sign ~p emit ~p",
             [
              maps:get(sign, SignedBlock),
              length(EmitTXs)
             ]),
  HBlk=msgpack:pack(
         #{null=><<"blockvote">>,
           <<"n">>=>node(),
           <<"hash">>=>maps:get(hash, SignedBlock),
           <<"sign">>=>maps:get(sign, SignedBlock),
           <<"chain">>=>MyChain
          }
        ),
  tpic:cast(tpic, <<"blockvote">>, HBlk),
  if EmitTXs==[] -> ok;
     true ->
       Push=gen_server:call(txpool, {push_etx, EmitTXs}),
       stout:log(push_etx,
                 [
                  {node_name,NodeName},
                  {txs, EmitTXs},
                  {res, Push}
                 ]),
       lager:info("Inject TXs ~p", [Push])
  end,
  {noreply, State#{preptxl=>[],
                   presig=>#{},
                   pre=>#{h=>PHeight, tmp=>PTmp}
                  }}
  catch throw:empty ->
        lager:info("Skip empty block"),
        {noreply, State#{preptxl=>[],
                         presig=>#{},
                         pre=>#{h=>PHeight, tmp=>PTmp}}}
    end;

handle_info(process, State) ->
    lager:notice("MKBLOCK Blocktime, but I not ready"),
    {noreply, load_settings(State)};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

sign(Blk, ED) when is_map(Blk) ->
    PrivKey=nodekey:get_priv(),
    block:sign(Blk, ED, PrivKey).

%% ------------------------------------------------------------------

load_settings(State) ->
    OldSettings=maps:get(settings, State, #{}),
    MyChain=blockchain:chain(),
    AE=chainsettings:get_val(<<"allowempty">>),
    NodeName=nodekey:node_name(),
    State#{
      settings=>
      maps:merge(
        OldSettings,
        #{ae=>AE, mychain=>MyChain, nodename=>NodeName}
      )
    }.

%% ------------------------------------------------------------------

decode_tpic_txs(TXs) ->
  lists:map(
    fun
      % get pre synced transaction body from txstorage
      ({TxID, null}) ->
        TxBody =
          case txstorage:get_tx(TxID) of
            {ok, {TxID, Tx, _Nodes}} ->
              Tx;
            error ->
              lager:error("can't get body for tx ~p", [TxID]),
              null
          end,
        {TxID, TxBody};
      
      % unpack transaction body
      ({TxID, Tx}) ->
        Unpacked = tx:unpack(Tx),
%%      lager:info("debug tx unpack: ~p", [Unpacked]),
        {TxID, Unpacked}
    end,
    maps:to_list(TXs)
  ).

