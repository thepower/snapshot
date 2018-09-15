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

-module(ledger_sync).

-export([run_source/4,
         run_target/4,
         target/5,
         synchronizer/5]).

call(TPIC, Handler, Object, Atoms) ->
    Res=tpic:call(TPIC, Handler, msgpack:pack(Object)),
    lists:filtermap(
      fun({Peer, Bin}) ->
              case msgpack:unpack(Bin, [{known_atoms, Atoms}]) of
                  {ok, Decode} ->
                      {true, {Peer, Decode}};
                  _ -> false
              end
      end, Res).

%%% sync destination
run_target(TPIC, PeerID, Ledger, BlockChainRDB) ->
    erlang:spawn(?MODULE, target,
                 [ TPIC, PeerID, Ledger, BlockChainRDB, self()]).

target(TPIC, PeerID, LedgerPID, _RDB, Parent) ->
    R=call(TPIC, PeerID,
           #{null=><<"instant_sync_run">>},
           []
          ),
    lager:debug("Sync tgt ~p", [R]),

    gen_server:call(LedgerPID, '_flush'),
    lager:debug("TgtSync start", []),
    Result0 = continue(TPIC, LedgerPID, R, Parent, [], []),
    {Result, Settings0}=lists:foldl(
                        fun({settings, List}, {RAcc, SAcc}) ->
                                {RAcc, [List|SAcc]};
                           (Res, {RAcc, SAcc}) ->
                                {[Res|RAcc], SAcc}
                        end, {[], []}, Result0),
    Settings=lists:flatten(Settings0),
    lager:debug("TgtSync done ~p", [Result]),
    Parent ! {inst_sync, settings, Settings},
    Parent ! {inst_sync, done, Result}.


continue(TPIC, LedgerPID, [{Handler, Res}], Parent, Acc, BlockAcc) ->
    lager:debug("sync continue ~p", [Res]),
    case Res of
        #{<<"done">> := _Done, <<"block">> := BlockPart} ->
            %#{hash:=Hash, header:=#{ledger_hash:=LH, height:=Height}}=Block=block:unpack(BinBlock),
            %lager:info("Got block ~p ~s~n", [Height, bin2hex:dbin2hex(Hash)]),
            %lager:info("Block's Ledger ~s~n", [bin2hex:dbin2hex(LH)]),
            <<Number:32, Length:32, _/binary>> = BlockPart,
            NewBlockAcc = [BlockPart|BlockAcc],
            if (length(NewBlockAcc) == Length) ->
                    BinBlock = block:glue_packet(NewBlockAcc),
                    %lager:debug("The block is ~p", [BinBlock]),
                    %lager:debug("unpacked block is ~p", [block:unpack(BinBlock)]),
                    Parent ! {inst_sync, block, BinBlock};
                true ->
                    lager:debug("Received part number ~p out of ~p", [Number, Length])
            end,
            R = call(TPIC, Handler, #{null => <<"continue">>}, []),
            continue(TPIC, LedgerPID, R, Parent, Acc, NewBlockAcc);
        #{<<"done">>:=_Done, <<"settings">>:=L} ->
            Parent ! {inst_sync, settings},
            R=call(TPIC, Handler, #{null=><<"continue">>}, []),
            continue(TPIC, LedgerPID, R, Parent, [{settings, L}|Acc], BlockAcc);
        #{<<"done">>:=Done, <<"ledger">>:=L} ->
            gen_server:call(LedgerPID,
                            {put, maps:fold(
                                    fun(K, V, A) ->
                                        CL=bal:unpack(V),
                                            [{K, CL}|A]
                                    end, [], L), ublk}),
            lager:info("L ~w~n", [maps:size(L)]),
            Parent ! {inst_sync, ledger},
            case Done of
                false  ->
                    R=call(TPIC, Handler, #{null=><<"continue">>}, []),
                    continue(TPIC, LedgerPID, R, Parent, Acc, BlockAcc);
                true ->
                    %{ok, C}=gen_server:call(LedgerPID, {check, []}),
                    %lager:info("My Ledger hash ~s", [bin2hex:dbin2hex(C)]),
                    [done|Acc]
            end;
        _Any ->
            lager:info("Unknown res ~p", [_Any]),
            [{error, unknown}|[Acc|BlockAcc]]
    end.



%%% sync source

run_source(TPIC, PeerID, LastBlock, Settings) ->
    {_, _}=Snap=gen_server:call(ledger, snapshot),
    erlang:spawn(?MODULE, synchronizer,
                 [ TPIC, PeerID, LastBlock, Snap, Settings]).

synchronizer(TPIC, PeerID,
             #{hash:=Hash, header:=#{height:=Height}}=Block,
             {DBH, Snapshot},
             Settings) ->
    {ok, Itr} = rocksdb:iterator(DBH, [{snapshot, Snapshot}]),
    Total=rocksdb:count(DBH),
    lager:info("TPIC ~p Peer ~p bh ~p, db ~p total ~p",
               [TPIC, PeerID, {Height, Hash}, {DBH, Snapshot}, Total]),
    Patches=settings:get_patches(Settings),
    lager:info("Patches ~p", [Patches]),
    %file:write_file("tmp/syncblock.txt",
    %                io_lib:format("~p.~n", [Block])),
    BlockParts = block:split_packet(block:pack(Block)),
    [BlockHead|BlockTail] = BlockParts,
    tpic:cast(TPIC, PeerID, msgpack:pack(#{done => false, block => BlockHead})),
    BlockSent = send_block(TPIC, PeerID, BlockTail),
    if BlockSent == done ->
        SP1 = send_settings(TPIC, PeerID, Patches),
        if SP1 == done ->
                SP2 = send_ledger({DBH, Snapshot}, TPIC, PeerID, first, Itr),
                lager:info("Sync finished: ~p / ~p", [SP1, SP2]);
            true ->
                lager:info("Sync interrupted while sending settings ~p", [SP1])
        end;
        true ->
            lager:info("Sync interrupted while sending block ~p", [BlockSent])
    end,
    rocksdb:release_snapshot(Snapshot).

pick_settings(Settings, N) ->
    LS=length(Settings),
    if(N>=LS) ->
          {Settings, []};
      true ->
          lists:split(N, Settings)
    end.

send_block(_, _, []) ->
    done;
send_block(TPIC, PeerID, Block) ->
    lager:info("send_block"),
    receive
        {'$gen_cast', {tpic, PeerID, Bin}} ->
            case msgpack:unpack(Bin) of
                {ok, #{null := <<"stop">>}} ->
                    tpic:cast(TPIC, PeerID, msgpack:pack(#{null => <<"stopped">>})),
                    interrupted;
                {ok, #{null := <<"continue">>}} ->
                    [ToSend|Rest] = Block,
                    lager:info("Sending block ~p", [ToSend]),
                    if (Rest == []) -> %last portion
                            Blob =# {done => false, block => ToSend},
                            tpic:cast(TPIC, PeerID, msgpack:pack(Blob)),
                            done;
                        true -> %Have more
                            Blob =# {done => false, block => ToSend},
                            tpic:cast(TPIC, PeerID, msgpack:pack(Blob)),
                            send_block(TPIC, PeerID, Rest)
                    end;
                {error, _} ->
                    error
            end;
        {'$gen_cast', Any} ->
            lager:info("Unexpected message ~p", [Any])
    after 30000 ->
        tpic:cast(TPIC, PeerID, msgpack:pack(#{null => <<"stopped">>})),
        timeout
    end.

send_settings(TPIC, PeerID, Settings) ->
    lager:info("send_settings"),
    receive
        {'$gen_cast', {tpic, PeerID, Bin}} ->
            case msgpack:unpack(Bin) of
                {ok, #{null:=<<"stop">>}} ->
                    tpic:cast(TPIC, PeerID, msgpack:pack(#{null=><<"stopped">>})),
                    interrupted;
                {ok, #{null:=<<"continue">>}} ->
                    {ToSend, Rest} = pick_settings(Settings, 5),
                    lager:info("Sending patches ~p", [ToSend]),
                    if(Rest == []) -> %last portion
                          Blob=#{done=>false, settings=>ToSend},
                          tpic:cast(TPIC, PeerID, msgpack:pack(Blob)),
                          done;
                      true -> %Have more
                          Blob=#{done=>false, settings=>ToSend},
                          tpic:cast(TPIC, PeerID, msgpack:pack(Blob)),
                          send_settings(TPIC, PeerID, Rest)
                    end;
                {error, _} ->
                    error
            end;
        {'$gen_cast', Any} ->
            lager:info("Unexpected message ~p", [Any])
    after 30000 ->
              tpic:cast(TPIC, PeerID, msgpack:pack(#{null=><<"stopped">>})),
              timeout
    end.

send_ledger(DB, TPIC, PeerID, Act, Itr) ->
    receive
        {'$gen_cast', {tpic, PeerID, Bin}} ->
            case msgpack:unpack(Bin) of
                {ok, #{null:=<<"stop">>}} ->
                    tpic:cast(TPIC, PeerID, msgpack:pack(#{null=><<"stopped">>})),
                    interrupted;
                {ok, #{null:=<<"continue">>}} ->
                    case pickx(DB, Act, Itr, 1000, []) of
                        {ok, L} ->
                            Blob=#{done=>false, ledger=>maps:from_list(L)},
                            tpic:cast(TPIC, PeerID, msgpack:pack(Blob)),
                            send_ledger(DB, TPIC, PeerID, next, Itr);
                        {error, L} ->
                            Blob=#{done=>true, ledger=>maps:from_list(L)},
                            tpic:cast(TPIC, PeerID, msgpack:pack(Blob)),
                            done
                    end;
                {error, _} ->
                    error
            end;
        {'$gen_cast', Any} ->
            lager:info("Unexpected message ~p", [Any])
    after 30000 ->
              tpic:cast(TPIC, PeerID, msgpack:pack(#{null=><<"stopped">>})),
              timeout
    end.


pickx(_, _, _, 0, A) -> {ok, A};
pickx({DBH, Snapshot}=DB, Act, Itr, N, A) ->
  case rocksdb:iterator_move(Itr, Act) of
    {ok, <<"lb:", _/binary>>, _} ->
      pickx(DB, next, Itr, N, A);
    {ok, <<"lastblk">>, _} ->
      pickx(DB, next, Itr, N, A);
    {ok, K, V} ->
      V0=binary_to_term(V),
      lager:notice("FX ME here"),
      V1=case rocksdb:get(DBH, <<"lb:", K/binary>>, [{snapshot, Snapshot}]) of
           {ok, LBH} ->
             lager:notice("LB ~p",[LBH]),
             V0#{ ublk=>LBH };
           _ ->
             lager:notice("LB none"),
             V0
         end,
      pickx(DB, next, Itr, N-1, [{K, bal:pack(V1,true)}|A]);
    {error, _} ->
      {error, A}
  end.

