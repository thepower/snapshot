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

-module(test_sync).
-export([run1/0, test1/0, candidates/0]).

call(Handler, Object, Atoms) ->
    io:format("Calling ~p~n", [Object]),
    Res=tpic:call(tpic, Handler, msgpack:pack(Object)),
    lists:filtermap(
      fun({Peer, Bin}) ->
              io:format("Response from ~p~n", [Peer]),
              case msgpack:unpack(Bin, [{known_atoms, Atoms}]) of
                  {ok, Decode} ->
                      {true, {Peer, Decode}};
                  _ -> false
              end
      end, Res).


checkcand(Cs) ->
    lists:filter( %first suitable will be the quickest
      fun({_Handler, #{chain:=_Ch,
                     last_hash:=_,
                     last_height:=_,
                     null:=<<"sync_available">>}}) -> true;
         (_) -> false
      end, Cs).

test1() ->
    %block by block synchromization
    [{Handler, Candidate}|_]=checkcand(
                               call(<<"blockchain">>,
                                 #{null=><<"sync_request">>},
                                 [last_hash, last_height, chain]
                                )),
    #{null:=Avail,
      chain:=Chain,
      last_hash:=Hash,
      last_height:=Height}=Candidate,
    io:format("~s chain ~w h= ~w hash= ~s ~n",
              [ Avail, Chain, Height, bin2hex:dbin2hex(Hash) ]),
    test1(Handler, Hash, 20).


test1(_, _, 0) ->
    done_limit;
test1(Handler, Hash, Rest) ->
    [{_, R}]=call(Handler,
                 #{null=><<"pick_block">>, <<"hash">>=>Hash, <<"rel">>=>prev},
                 [block]
                ),
    case maps:is_key(block, R) of
        false ->
            done_no_block;
        true ->
            BinBlk=maps:get(block, R),
            Blk=block:unpack(BinBlk),
            #{header:=#{height:=Height}=Hdr, hash:=HH}=Blk,
            io:format("Res ~p~nBlock ~6w (~6w KB) ~s~n",
                      [ maps:without([<<"req">>, block], R),
                        Height,
                        size(BinBlk) div 1024,
                        bin2hex:dbin2hex(HH)
                      ]),
            case maps:is_key(parent, Hdr) of
                true ->
                    test1(Handler, HH, Rest-1);
                false ->
                    done_no_parent
            end
    end.


%run() ->
%    %instant synchronization
%    [{Handler, Candidate}|_]=checkcand(call(<<"blockchain">>,
%                                 #{null=><<"sync_request">>},
%                                 [last_hash, last_height, chain]
%                                )),
%    #{null:=Avail,
%      chain:=Chain,
%      last_hash:=Hash,
%      last_height:=Height}=Candidate,
%    io:format("~s chain ~w h= ~w hash= ~s ~n",
%              [ Avail, Chain, Height, bin2hex:dbin2hex(Hash) ]),
%    io:format("Handler ~p~n", [Handler]),
%    R=call(Handler,
%           #{null=><<"instant_sync_run">>},
%           []
%          ),
%
%    Name=test_sync_ledger,
%    {ok, Pid}=ledger:start_link(
%               [{filename, "db/ledger_test_syncx"},
%                {name, Name}
%               ]
%              ),
%    gen_server:call(Pid, '_flush'),
%    Result=cont(R),
%    {ok, C}=gen_server:call(test_sync_ledger, {check, []}),
%    gen_server:cast(Pid, terminate),
%    io:format("My Ledger ~s~n", [bin2hex:dbin2hex(C)]),
%    Result.

run1() ->
    %instant synchronization
    [{Handler, Candidate}|_]=checkcand(call(<<"blockchain">>,
                                 #{null=><<"sync_request">>},
                                 [last_hash, last_height, chain]
                                )),
    #{null:=Avail,
      chain:=Chain,
      last_hash:=Hash,
      last_height:=Height}=Candidate,
    io:format("~s chain ~w h= ~w hash= ~s ~n",
              [ Avail, Chain, Height, bin2hex:dbin2hex(Hash) ]),

    Name=test_sync_ledger,
    {ok, Pid}=ledger:start_link(
               [{filename, "db/ledger_test_syncx2"},
                {name, Name}
               ]
              ),
    gen_server:call(Pid, '_flush'),

    ledger_sync:run_target(tpic, Handler, Pid, undefined),

    R=wait_more(),
    gen_server:cast(Pid, terminate),
    R.

candidates() ->
    Candidates=call(<<"blockchain">>,
                    #{null=><<"sync_request">>},
                    [last_hash, last_height, chain]
                   ),
    lists:foreach(
      fun({Handle, Info}) ->
              #{null:=Avail,
                chain:=Chain,
                last_hash:=Hash,
                last_height:=Height}=Info,
              io:format("~p ~s chain ~w h= ~w hash= ~s ~n",
                        [ Handle, Avail, Chain, Height, bin2hex:dbin2hex(Hash) ])
      end, Candidates).


wait_more() ->
    receive
        {inst_sync, block, _} ->
            io:format("B"),
            wait_more();
        {inst_sync, settings} ->
            io:format("s"),
            wait_more();
        {inst_sync, ledger} ->
            io:format("L"),
            wait_more();
        {inst_sync, settings, _} ->
            io:format("S"),
            wait_more();
        {inst_sync, done, Res} ->
            io:format("~n~p~n", [Res]),
            ok;
        Any ->
            io:format("error: ~p", [Any]),
            {error, Any}
    after 10000 ->
              timeout
    end.

%cont([{Handler, Res}]) ->
%    case Res of
%        #{<<"block">>:=BinBlock} ->
%            io:format("Block ~p~n",[msgpack:unpack(BinBlock)]),
%            #{hash:=Hash, header:=#{ledger_hash:=LH, height:=Height}}=block:unpack(BinBlock),
%            io:format("Got block ~p ~s~n", [Height, bin2hex:dbin2hex(Hash)]),
%            io:format("Block's Ledger ~s~n", [bin2hex:dbin2hex(LH)]),
%            R=call(Handler, #{null=><<"continue">>}, []),
%            cont(R);
%        #{<<"done">>:=false, <<"ledger">>:=L} ->
%            l_apply(L),
%            io:format("L ~w~n", [maps:size(L)]),
%            R=call(Handler, #{null=><<"continue">>}, []),
%            cont(R);
%        #{<<"done">>:=true, <<"ledger">>:=L} ->
%            l_apply(L),
%            io:format("L ~w~n", [maps:size(L)]),
%            io:format("Done~n"),
%            done
%    end.
%
%
%l_apply(L) ->
%    gen_server:call(test_sync_ledger,
%                    {put, maps:fold(
%                            fun(K, V, A) ->
%                                    [{K, bal:unpack(V)}|A]
%                            end, [], L)}).
%
