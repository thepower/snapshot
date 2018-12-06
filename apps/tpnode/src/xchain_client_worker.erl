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

-module(xchain_client_worker).
-export([start_link/1,start_link/2,run/2,ws_mode/3]).

start_link(Sub) ->
  GetFun=fun(node_id) ->
             nodekey:node_id();
            (chain) ->
             blockchain:chain();
            ({apply_block,#{hash:=_H}=Block}) ->
             txpool:inbound_block(Block);
            ({last,ChainNo}) ->
             Last=chainsettings:by_path([
                                         <<"current">>,
                                         <<"sync_status">>,
                                         xchain:pack_chid(ChainNo),
                                         <<"block">>]),
             lager:info("Last known to ch ~b: ~p",[ChainNo,Last]),
             Last
         end,
  Pid=spawn(xchain_client_worker,run,[Sub#{parent=>self()}, GetFun]),
  link(Pid),
  {ok, Pid}.

start_link(Sub,test) ->
  GetFun=fun(node_id) ->
             nodekey:node_id();
            (chain) -> 5;
            ({apply_block,#{txs:=TL,header:=H}=_Block}) ->
             io:format("TXS ~b Hei: ~p~n",[length(TL),maps:get(height,H)]),
             ok;
            ({last, 4}) ->
             hex:parse("A3366B6B16F58D684415EE150B055E9309578C954D0480CF25E796EB83D6FFEA");
            ({last,_ChainNo}) ->
             undefined;
            (Any) ->
             io:format("-------~n ERROR ~n~p~n-------~n",[Any]),
             undefined
         end,
  Pid=spawn(xchain_client_worker,run,[Sub#{parent=>self()}, GetFun]),
  link(Pid),
  {ok, Pid}.

run(#{parent:=Parent, address:=Ip, port:=Port} = Sub, GetFun) ->
  {ok, _} = application:ensure_all_started(gun),
  process_flag(trap_exit, true),
  try
    lager:info("xchain client connecting to ~p ~p", [Ip, Port]),
    {ok, Pid} = gun:open(Ip, Port),
    receive
      {gun_up, Pid, http} ->
        ok
    after 20000 ->
            gun:close(Pid),
            throw('up_timeout')
    end,
    Proto=case sync_get_decode(Pid, "/xchain/api/compat.mp") of
            {200, _, #{<<"ok">>:=true,<<"version">>:=Ver}} -> Ver;
            {404, _, _} -> 0;
            _ -> 0
          end,
    {ok,UpgradeHdrs}=upgrade(Pid,Proto),
    lager:debug("Conn upgrade hdrs: ~p",[UpgradeHdrs]),
    #{null:=<<"iam">>,
      <<"chain">>:=HisChain,
      <<"node_id">>:=NodeID}=reg(Pid, Proto, GetFun),
    Parent ! {wrk_up, self(), NodeID},
    Known=GetFun({last, HisChain}),
    MyChain=GetFun(chain),
    BlockList=block_list(Pid, Proto, GetFun(chain), last, Known, []),
    lists:foldl(
      fun({He,Ha},_) ->
          lager:info("Blk ~b hash ~p~n",[He,Ha])
      end, 0, BlockList),
    lists:foldl(
      fun(_,Acc) when is_atom(Acc) ->
          Acc;
         ({_Height, BlkID},0) when BlkID == Known ->
          lager:info("Skip known block ~s",[hex:encode(BlkID)]),
          0;
         ({_Height, BlkID},_) when BlkID == Known ->
          lager:info("Skip known block ~s",[hex:encode(BlkID)]),
          error;
         ({0, <<0,0,0,0,0,0,0,0>>}, Acc) ->
          Acc;
         ({_Height, BlkID},Acc) ->
          lager:info("Pick block ~s",[hex:encode(BlkID)]),
          case pick_block(Pid, Proto, MyChain, BlkID) of
            #{null:=<<"owblock">>,
              <<"ok">>:=true,
              <<"block">>:=Blk} ->
              #{txs:=_TL,header:=_}=Block=block:unpack(Blk),
              ok=GetFun({apply_block, Block}),
              Acc+1;
            #{null := <<"owblock">>,<<"ok">> := false} ->
              lager:info("Fail block ~s",[hex:encode(BlkID)]),
              fail
          end
      end, 0, BlockList),
    [<<"subscribed">>,_]=make_ws_req(Pid, Proto,
                                     #{null=><<"subscribe">>,
                                       <<"channel">>=>xchain:pack_chid(MyChain)}
                                    ),
    %io:format("SubRes ~p~n",[SubRes]),
    ok=gun:ws_send(Pid, {binary, xchain:pack(#{null=><<"ping">>}, Proto)}),
    ws_mode(Pid,Sub#{proto=>Proto},GetFun),
    gun:close(Pid),
    done
  catch
    throw:up_timeout ->
      Parent ! {wrk_down, self(), error},
      lager:debug("connection to ~p was timed out", [Sub]),
      pass;
    Ec:Ee ->
          Parent ! {wrk_down, self(), error},
          S=erlang:get_stacktrace(),
          lager:error("xchain client error ~p:~p",[Ec,Ee]),
          lists:foreach(
            fun(SE) ->
                lager:error("@ ~p", [SE])
            end, S)
  end.

ws_mode(Pid, #{parent:=Parent, proto:=Proto}=Sub, GetFun) ->
  receive
    {'EXIT',_,shutdown} ->
      Cmd = xchain:pack(#{null=><<"goodbye">>, <<"r">>=><<"shutdown">>}, Proto),
      gun:ws_send(Pid, {binary, Cmd}),
      gun:close(Pid),
      exit;
    {'EXIT',_,Reason} ->
      lager:error("Linked process went down ~p. Giving up....",[Reason]),
      Cmd = xchain:pack(#{null=><<"goodbye">>, <<"r">>=><<"deadparent">>}, Proto),
      gun:ws_send(Pid, {binary, Cmd}),
      gun:close(Pid),
      exit;
    {state, CPid} ->
      CPid ! {Pid, Sub},
      ?MODULE:ws_mode(Pid, Sub, GetFun);
    stop ->
      Cmd = xchain:pack(#{null=><<"goodbye">>, <<"r">>=><<"stop">>}, Proto),
      gun:ws_send(Pid, {binary, Cmd}),
      gun:close(Pid),
      Parent ! {wrk_down, self(), stop},
      done;
    {send_msg, Payload} ->
      Cmd = xchain:pack(Payload, Proto),
      gun:ws_send(Pid, {binary, Cmd}),
      ?MODULE:ws_mode(Pid, Sub, GetFun);
    {gun_ws, Pid, {binary, Bin}} ->
      Cmd = xchain:unpack(Bin, Proto),
      lager:debug("XChain client got ~p",[Cmd]),
      Sub1=xchain_client_handler:handle_xchain(Cmd, Pid, Sub),
      ?MODULE:ws_mode(Pid, Sub1, GetFun);
    {gun_down,Pid,ws,closed,[],[]} ->
      lager:error("Gun down. Giving up...."),
      Parent ! {wrk_down, self(), gundown},
      giveup;
    Any ->
      lager:notice("XChain client unknown msg ~p",[Any]),
      ?MODULE:ws_mode(Pid, Sub, GetFun)
  after 60000 ->
          ok=gun:ws_send(Pid, {binary, xchain:pack(#{null=><<"ping">>}, Proto)}),
          ?MODULE:ws_mode(Pid, Sub, GetFun)
  end.

reg(Pid, Proto, GetFun) ->
  MyNodeId = GetFun(node_id),
  Chain = GetFun(chain),
  make_ws_req(Pid, Proto, #{
                     null=><<"node_id">>,
                     node_id=>MyNodeId,
                     chain=>Chain
                    }).

pick_block(Pid, Proto, Chain, Parent) ->
  make_ws_req(Pid, Proto, #{null=><<"owblock">>,
                            <<"chain">>=>Chain,
                            <<"parent">>=>Parent
                           }).

block_list(_, _, _, Last, Known, Acc) when Last==Known ->
  Acc;

block_list(Pid, Proto, Chain, Last, Known, Acc) ->
  Req=if Last==last ->
           lager:debug("Blocklist last",[]),
           #{null=><<"last_ptr">>, <<"chain">>=>Chain};
         true ->
           lager:debug("Blocklist ~s",[hex:encode(Last)]),
           #{null=><<"pre_ptr">>,  <<"chain">>=>Chain, <<"block">>=>Last}
    end,
  R=make_ws_req(Pid, Proto, Req),
  lager:debug("Got block_list resp ~p",[R]),
  case R of
    #{null := N,
      <<"ok">> := true,
      <<"pointers">> := #{<<"height">> := H,
                          <<"hash">> := P,
                          <<"pre_height">> := HH,
                          <<"pre_hash">> := PP}} 
      when is_binary(P) 
           andalso 
           (N==<<"last_ptr">> orelse N==<<"pre_ptr">>) ->
      if(PP==Known) ->
          [{HH,PP},{H,P}|Acc];
        (P==Known) ->
          [{H,P}|Acc];
        true ->
          block_list(Pid, Proto, Chain, PP, Known, [{HH,PP},{H,P}|Acc])
      end;
    #{null := N,
      <<"ok">> := true,
      <<"pointers">> := #{<<"pre_height">> := HH,
                          <<"pre_hash">> := PP}} when N==<<"last_ptr">> orelse
                                                      N==<<"pre_ptr">> ->
      if(PP==Known) ->
          [{HH,PP}|Acc];
        true ->
          block_list(Pid, Proto, Chain, PP, Known, [{HH,PP}|Acc])
      end;
    #{null := N,
      <<"ok">> := true,
      <<"pointers">> := #{<<"height">> := H,
                          <<"hash">> := P}} 
      when is_binary(P) 
           andalso 
           (N==<<"last_ptr">> orelse N==<<"pre_ptr">>) ->
      [{H,P}|Acc];
    Any ->
      lager:info("Err ~p",[Any]),
      lager:info("Acc is  ~p",[Acc]),
      Acc
  end.

make_ws_req(Pid, Proto, Request) ->
  receive {gun_ws,Pid, {binary, _}} ->
            throw('unexpected_data')
  after 0 -> ok
  end,
  Cmd = xchain:pack(Request, Proto),
  ok=gun:ws_send(Pid, {binary, Cmd}),
  receive {gun_ws,Pid, {binary, Payload}}->
            {ok, Res} = msgpack:unpack(Payload),
            Res
  after 5000 ->
          throw('ws_timeout')
  end.

upgrade(Pid, 2) ->
  gun:ws_upgrade(Pid, "/xchain/ws",
                 [ {<<"sec-websocket-protocol">>, <<"thepower-xchain-v2">>} ]),
  receive {gun_ws_upgrade,Pid,Status,Headers} ->
            {Status, Headers}
  after 10000 ->
          throw(upgrade_timeout)
  end.

sync_get_decode(Pid, Url) ->
  {Code,Header, Body}=sync_get(Pid, Url),
  case proplists:get_value(<<"content-type">>,Header) of
    <<"application/json">> ->
      {Code, Header, jsx:decode(iolist_to_binary(Body), [return_maps])};
    <<"application/msgpack">> ->
      {ok, Res}=msgpack:unpack(iolist_to_binary(Body)),
      {Code, Header, Res};
    _ ->
      {Code, Header, Body}
  end.

sync_get(Pid, Url) ->
  Ref=gun:get(Pid,Url),
  sync_get_continue(Pid, Ref, {0,[],[]}).

sync_get_continue(Pid, Ref, {PCode,PHdr,PBody}) ->
  {Fin,NS}=receive
             {gun_response,Pid,Ref,IsFin, Code, Headers} ->
               {IsFin, {Code,Headers,PBody} };
             {gun_data,Pid,Ref, IsFin, Payload} ->
               {IsFin, {PCode,PHdr,PBody++[Payload]}}
           after 10000 ->
                   throw(get_timeout)
           end,
  case Fin of
    fin ->
      NS;
    nofin ->
      sync_get_continue(Pid, Ref, NS)
  end.
