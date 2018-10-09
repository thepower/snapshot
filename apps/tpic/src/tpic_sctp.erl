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

-module(tpic_sctp).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/inet_sctp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,resolve/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Settings) when is_map(Settings) ->
  gen_server:start_link({local, tpic}, ?MODULE, Settings, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%streams:
%0 - socket control and neighbor discovery
%1 - real time (synchronization)

init([]) ->
  throw('loadconfig');

init(#{port:=MyPort,handler:=TH}=Args) when is_map(Args) ->
  {ok,S} = gen_sctp:open(MyPort, [{recbuf,maps:get(recbuf,Args,655360)},
                                  {ip,any},
                                  {active,true},
                                  inet6,
                                  {reuseaddr, true},
                                  {sctp_nodelay, true},
                                  {sctp_i_want_mapped_v4_addr, true},
                                  {sndbuf,maps:get(sndbuf,Args,655360)}]),
  lager:info("Listening on ~p Socket ~p", [MyPort,S]),
  ok=gen_sctp:listen(S, true),
  erlang:send_after(1000,self(), connect_peers),
  PeerInfo = resolve_addresses(init, maps:get(peers, Args, [])),
  %lager:info("Init PeerInfo ~p",[PeerInfo]),
  {_,ExtRouting}=maps:fold(
                   fun (K,_V,{L,A}) ->
                       case maps:is_key(K,A) of
                         false ->
                           {L+1,maps:put(K,L,A)};
                         true ->
                           {K, A}
                       end
                   end,{2,#{<<"timesync">>=>1}}, TH:routing(Args)),
  RRouting=maps:fold(
             fun(K,V,A) ->
                 maps:put(V,K,A)
             end, #{}, ExtRouting),
  {ok, Args#{
         socket=>S,
         extrouting=>ExtRouting,
         rrouting=>RRouting,
         peerinfo=>PeerInfo,
         lastreqid=>1,
         nodeid=> try
                    nodekey:get_pub()
                  catch _:_ -> atom_to_binary(node(),utf8)
                  end,
         reqmap=>hashqueue:new()
        }
  }.

handle_call(peers, _From, #{peerinfo:=PeerInfo}=State) ->
  {reply, PeerInfo, State};

handle_call({add_peers, NewPeers}, _From, #{peerinfo:=PeerInfo}=State) ->
  {reply, ok, State#{ peerinfo => add_peers(PeerInfo, NewPeers) }};

handle_call(state, _From, State) ->
  {reply, State, State};

handle_call({broadcast, Chan}, _From,
            #{peerinfo:=PeerInfo}=State) when is_binary(Chan) orelse Chan==service ->
  Sent=mkmap:fold(
         fun(K,#{routes:=Routes}=_Ctx,Acc) ->
             case proplists:get_value(ass,K) of
               undefined -> Acc; %Not connected
               Assoc -> %connected
                 SID=case maps:is_key(Chan,Routes) of
                       true ->
                         maps:get(Chan,Routes);
                       _ when Chan==service ->
                         0;
                       false -> undefined
                     end,
                 if(is_integer(SID)) ->
                     [{{Assoc,SID},_Ctx}|Acc];
                   true ->
                     %no such channel
                     Acc
                 end
             end;
            (_,_,Acc) ->
             Acc
         end, [], PeerInfo),
  {reply, Sent, State};


handle_call({broadcast, OPid, Chan, Message}, _From,
            #{nodeid:=Node,reqmap:=HQ,lastreqid:=LR,peerinfo:=PeerInfo,socket:=Socket}=State) ->
  MsgID=mkreqid(Node,LR),
  Sent=mkmap:fold(
         fun(K,#{routes:=Routes}=_Ctx,Acc) ->
             case proplists:get_value(ass,K) of
               undefined -> Acc; %Not connected
               Assoc -> %connected
                 SID=case maps:is_key(Chan,Routes) of
                       true ->
                         maps:get(Chan,Routes);
                       _ when Chan==service ->
                         0;
                       false -> undefined
                     end,

                 if(is_integer(SID)) ->
                     R=gen_sctp:send(Socket, Assoc, SID,
                                     encapmsg(MsgID,Message)),
                     if R==ok ->
                          lager:debug("I have assoc ~p, MsgID ~p send ~p",[Assoc,MsgID,R]),
                          [{Assoc,SID,MsgID}|Acc];
                        true ->
                          S=erlang:get_stacktrace(),
                          lager:error("Send failed ~p",[R]),
                          lists:foreach(
                            fun(Se) ->
                                lager:error("at ~p",[Se])
                            end, S),
                          Acc
                     end;
                   true ->
                     %no such channel
                     Acc
                 end
             end;
            (_,_,Acc) ->
             Acc
         end, [], PeerInfo),
  lager:debug("Broadcast ~p ~p bytes: ~p",[Chan,size(Message),Sent]),
  if Sent==[] ->
       {reply, Sent, State};
     true ->
       lager:debug("AddRM: msg ~p origin PID ~p",[MsgID, OPid]),
       HQ1=hashqueue:add(MsgID,erlang:system_time(second),OPid,HQ),
       {reply, Sent, State#{
                       lastreqid=>LR+1,
                       reqmap=>HQ1
                      }
       }
  end;

handle_call({unicast, OPid, {Assoc, SID, MsgID}, Message}, _From,
            #{peerinfo:=PeerInfo,reqmap:=HQ,socket:=Socket}=State) when is_binary(MsgID) ->
  case mkmap:get({ass,Assoc}, PeerInfo, undefined) of
    undefined ->
      {reply, [], State};
    #{} ->
      lager:debug("Sending msg ~p",[Message]),
      R=gen_sctp:send(Socket, Assoc, SID,
                      encapmsg(MsgID, Message)),
      lager:debug("I have assoc ~p, send ~p",[Assoc,R]),
      if R==ok ->
           HQP=hashqueue:get(MsgID,HQ),
           if HQP == OPid ->
                lager:debug("SeenRM: msg ~p origin PID ~p",[MsgID, OPid]),
                %still valid translation
                %TODO: check validity period and prolongate?
                {reply, [{Assoc,SID,MsgID}], State};
              true ->
                lager:debug("ReaddRM: msg ~p origin PID ~p",[MsgID, OPid]),
                %no valid anymore, reinsert
                {reply, [{Assoc,SID,MsgID}],
                 State#{
                   reqmap=>hashqueue:add(MsgID,erlang:system_time(second),OPid,HQ)
                  }}
           end;
         true ->
           lager:debug("Send failed ~p",[R]),
           {reply, [], State}
      end
  end;

handle_call({unicast, OPid, {Assoc, SID}, Message}, _From,
            #{nodeid:=Node,reqmap:=HQ,lastreqid:=LR,peerinfo:=PeerInfo,socket:=Socket}=State) ->
  case mkmap:get({ass,Assoc}, PeerInfo, undefined) of
    undefined ->
      {reply, [], State};
    #{} ->
      MsgID=mkreqid(Node,LR),
      R=gen_sctp:send(Socket, Assoc, SID, encapmsg(MsgID, Message)),
      lager:debug("I have assoc ~p, send ~p",[Assoc,R]),
      if R==ok ->
           lager:debug("AddRM: msg ~p origin PID ~p",[MsgID, OPid]),
           {reply, [{Assoc,SID,MsgID}],
            State#{
              lastreqid=>LR+1,
              reqmap=>hashqueue:add(MsgID,erlang:system_time(second),OPid,HQ)
             }};
         true ->
           lager:debug("Send failed ~p",[R]),
           {reply, [], State}
      end
  end;

handle_call(_Request, _From, State) ->
  lager:notice("TPIC_SCTP unknown call ~p",[_Request]),
  {reply, unhandled, State}.

handle_cast({broadcast, _OPid, _Chan, _Message}=Req, State) ->
  {reply, _, State2} = handle_call(Req,undef,State),
  {noreply, State2};

handle_cast({unicast, _OPid, _Chan, _Message}=Req, State) ->
  {reply, _, State2} = handle_call(Req,undef,State),
  {noreply, State2};

handle_cast(_Msg, State) ->
  lager:notice("TPIC_SCTP unknown cast ~p",[_Msg]),
  {noreply, State}.

%{sctp,Socket,IP,Port,{[{sctp_sndrcvinfo,0,0,[],0,0,0,0,396599080,3}],{sctp_assoc_change,comm_up,0,10,5,3}}}
%{sctp,Socket,IP,Port,{[{sctp_sndrcvinfo,0,0,[],0,0,0,0,396599080,3}],{sctp_paddr_change,{{0,0,0,0,0,0,0,1},1234},addr_confirmed,0,3}}}
%{sctp,Socket,IP,Port,{[{sctp_sndrcvinfo,0,0,[],0,0,0,396599081,396599081,3}],<<"Test 0">>}}

handle_info(connect_peers, #{reqmap:=HQ,peerinfo:=PeerInfo,socket:=S}=State) ->
  lager:debug("Connect peers"),
  lists:foldl(
    fun([{Host,Port}|_]=K,Acc) ->
        try
          case proplists:get_value(ass,K) of
            undefined ->
              ok;
            _ ->
              throw(connected)
          end,

          T=mkmap:get({Host,Port}, PeerInfo),
          case maps:get(auth,T,undefined) of
            fail ->
              TD=erlang:system_time(seconds)-maps:get(lastdown,T,0),
              if TD<60 ->
                   throw({skip,authfail});
                 true -> ok
              end;
            _ -> ok
          end,

          lager:debug("I have to connect to ~p: ~p",[Host,T]),
          case gen_sctp:connect_init
               (S, Host, Port, [{sctp_initmsg,#sctp_initmsg{num_ostreams=10}}])
          of ok ->
               lager:info("Trying to connect ~p: ~p",[Host,T]),
               ok;
             {error,ealready} ->
               %wait a little bit and association will up
               throw({skip,ealready});
             {error,EReason} ->
               lager:notice("unknown connect error [ ~p ]", [EReason]),
               throw({skip,ealready})
          end,
          %lager:info("Outcoing assoc ~p",[Assoc]),
          Acc

        catch
          throw:connected ->
            Acc;
          throw:{skip,Reason}->
            lager:info("Skip ~p: ~p",[{Host,Port},Reason]),
            Acc
        end
    end, undefined, mkmap:get_all_keys(PeerInfo)),

  erlang:send_after(10000,self(), connect_peers),
  HQ1=clean_reqmap(HQ),
  {noreply, State#{reqmap=>HQ1}};


handle_info({sctp,Socket,RemoteIP,RemotePort,
             { [AAnc]=Anc, Payload }
            }, #{socket:=Socket}=State) when is_binary(Payload) ->
  showanc(Anc,RemoteIP,RemotePort),
  State1=handle_payload(AAnc,{RemoteIP,RemotePort},Payload,State),
  {noreply, State1};

handle_info({sctp,Socket,RemoteIP,RemotePort,
             { Anc, #sctp_assoc_change{
                       assoc_id=AID,
                       error=Err,
                       outbound_streams=OS,
                       inbound_streams=IS,
                       state=ASt
                      }
             }
            }, #{peerinfo:=PeerInfo,socket:=Socket}=State) ->
  showanc(Anc,RemoteIP,RemotePort),
  lager:debug("AssChange for ~p:~w",[RemoteIP,RemotePort]),
  Action=case ASt of
           comm_up -> up;
           cant_assoc -> error;
           comm_lost -> down;
           restart -> down;
           shutdown_comp -> down
         end,
  lager:debug("AssocChange ~w ~s (IS ~w OS ~w Err ~w)",[AID,ASt,IS,OS,Err]),
  PeerInfo1=case Action of
              up ->
                Challenge= <<"Hello",(crypto:strong_rand_bytes(16))/binary,"\r\n">>,
                gen_sctp:send(Socket, AID, 0, Challenge),
                Pre=mkmap:get({RemoteIP,RemotePort}, PeerInfo, #{}),
                mkmap:add_key({RemoteIP,RemotePort},{ass,AID},
                              mkmap:put({RemoteIP,RemotePort},
                                        maps:merge(Pre,
                                                   #{auth=>undefined,
                                                     lastup=>erlang:system_time(seconds),
                                                     prim=>{RemoteIP,RemotePort},
                                                     mych=>Challenge,
                                                     state=>init
                                                    }
                                                  ), PeerInfo)
                             );
              down ->
                Pre=mkmap:get({ass, AID}, PeerInfo, #{}),
                mkmap:del_key({ass,AID},
                              mkmap:put({RemoteIP,RemotePort},
                                        maps:merge(
                                          Pre,
                                          #{state=>down,
                                            lastdown=>erlang:system_time(seconds)
                                           }
                                         ), PeerInfo)
                             );
              error ->
                PeerInfo
            end,

  lager:debug("PeerInfo ~p",[PeerInfo1]),
  {noreply, State#{peerinfo=>PeerInfo1}};

handle_info({sctp,Socket,RemoteIP,RemotePort,
             { Anc, #sctp_paddr_change{assoc_id=AID,state=ASt,addr={Addr,AddrP},error=Err} }
            }, #{peerinfo:=PeerInfo, socket:=Socket}=State) ->
  showanc(Anc,RemoteIP,RemotePort),

  Action=case ASt of
           addr_unreachable -> del;
           addr_available -> add;
           addr_removed -> del;
           addr_added -> add;
           addr_made_prim -> prim;
           addr_confirmed -> add
         end,

  lager:debug("AddrChange ~w ~s ~s (err ~p)",[AID,ASt,inet:ntoa(Addr),Err]),

  PeerInfo1=case Action of
              prim ->
                Pre=mkmap:get({ass, AID}, PeerInfo),
                mkmap:put({ass,AID},
                          maps:merge(
                            Pre,
                            #{ prim=>{Addr,AddrP} }
                           )
                          , PeerInfo);
              add ->
                lager:debug("Add address ~p to ~p",[Addr,AID]),
                mkmap:add_key({ass,AID},{Addr,AddrP}, PeerInfo);
              del ->
                try
                  mkmap:del_key({Addr,AddrP}, PeerInfo)
                catch error:{badkey,_} ->
                        PeerInfo
                end
            end,

  lager:debug("PeerInfo ~p",[PeerInfo1]),

  {noreply, State#{peerinfo=>PeerInfo1}};

handle_info({sctp,Socket,RemoteIP,RemotePort, Message }, #{socket:=Socket}=State) ->
  lager:notice("SCTP from ~s:~w unknown ~p",[
                                             inet:ntoa(RemoteIP),
                                             RemotePort,
                                             Message
                                            ]),
  {noreply, State};

handle_info(_Info, State) ->
  lager:notice("unknown info ~p",[_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
mapto6({_,_,_,_,_,_,_,_}=IP6) ->
  IP6;
mapto6({A,B,C,D}) ->
  {0,0,0,0,0,16#FFFF,(A bsl 8) bor B,(C bsl 8) bor D}.

resolve(Host) ->
  lists:foldl(
    fun(AF,A) ->
        case inet:gethostbyname(Host,AF) of
          {ok, #hostent{h_addr_list=P}} ->
            A++lists:map(fun mapto6/1,P);
          {error, _} ->
            A
        end
    end,
    [], [inet6,inet]).

showanc([],RemoteIP,RemotePort) ->
  lager:debug("from ~s:~w unknown",
              [ inet:ntoa(RemoteIP), RemotePort ]);

showanc([#sctp_sndrcvinfo{
            stream=SID,    % 0	Streams numbered from 0 (XXX?)
            flags=Flags,   % [unordered, addr_over, abort, eof]
            ppid=PPID,     % 0,	Passed to the remote end
            context=Ctx,   % 0,	Passed to the user on error
            cumtsn=Cumtsn, % 0,	Only for unordered recv
            assoc_id=Assoc % 0		IMPORTANT!
           }],RemoteIP,RemotePort) ->
  lager:debug("from ~s:~w ass ~w str ~w flg ~p pp ~p ctx ~p cum ~p",
              [ inet:ntoa(RemoteIP), RemotePort,
                Assoc, SID, Flags, PPID, Ctx, Cumtsn ]).

handle_payload(#sctp_sndrcvinfo{ stream=SID, assoc_id=Assoc }=Anc,
               {RemoteIP,RemotePort},Payload,#{peerinfo:=PeerInfo,socket:=Socket}=State) ->
  Ctx=mkmap:get({ass,Assoc}, PeerInfo, #{}),
  lager:debug("++> Payload from ~p ~p: ~p  (ctx ~p)", [Assoc, SID, Payload, Ctx]),
  case payload(Socket, Anc, {RemoteIP,RemotePort},Payload,Ctx,State) of
    {ok, close} ->
      gen_sctp:eof(Socket, #sctp_assoc_change{
                              assoc_id = Assoc
                             }),
      State;
    {ok, NewState} ->
      NewState;
    {ok, NewCtx, NewState} ->
      PeerInfo1=mkmap:put({ass,Assoc},NewCtx, PeerInfo),
      NewState#{ peerinfo=>PeerInfo1 }
      %        _Any ->
      %            lager:error("Bad return from payload handler ~p",[_Any]),
      %            State
  end.

payload(Socket,
        #sctp_sndrcvinfo{ stream=SID, assoc_id=Assoc },
        _Remote,Payload,#{state := init}=Ctx,State) when SID==0 ->
  lager:debug("Make chresp"),
  ExtAuth=case maps:get(authmod,State,undefined) of
            X when is_atom(X) ->
              case erlang:function_exported(X,authgen,2) of
                true ->
                  X:authgen(Payload,maps:get(authctx,State,#{}));
                false ->
                  undefined
              end;
            _ ->
              lager:notice("No authmod specified"),
              undefined
          end,
  if is_binary(ExtAuth) ->
       gen_sctp:send(Socket, Assoc, SID, ExtAuth),
       {ok, Ctx#{state=>chrespo}, State};
     ExtAuth == undefined ->
       gen_sctp:send(Socket, Assoc, SID, crypto:hash(sha,Payload)),
       {ok, Ctx#{state=>chrespo}, State};
     true ->
       lager:error("tpic Auth module returned unexpected result ~p",
                   [ExtAuth]),
       {ok, close}
  end;

payload(Socket,
        #sctp_sndrcvinfo{ stream=SID, assoc_id=Assoc },
        _Remote,Payload,#{state := chrespo, mych:=Ch}=Ctx,State) when SID==0 ->
  ExtAuth=case maps:get(authmod,State,undefined) of
            X when is_atom(X) ->
              case erlang:function_exported(X,authcheck,3) of
                true ->
                  X:authcheck(Ch,Payload,maps:get(authctx,State,#{}));
                false ->
                  undefined
              end;
            _ ->
              lager:notice("No authmod specified"),
              undefined
          end,
  {Auth,AuthData}=case ExtAuth of
                    true -> {true,undefined};
                    false -> {false,undefined};
                    {true, AD} -> {true, AD};
                    {false, AD} -> {false, AD};
                    undefined ->
                      {crypto:hash(sha,Ch)==Payload,undefined};
                    _ ->
                      lager:error("tpic Auth module returned unexpected result ~p",
                                  [ExtAuth]),
                      {false,undefined}
                  end,
  if Auth==true ->
       lager:debug("Auth ok"),
       Routing=msgpack:pack(#{
                 null=><<"routing">>,
                 <<"routing">>=>maps:get(extrouting,State)
                }),
       lager:debug("Send routing ~p",[Routing]),
       gen_sctp:send(Socket, Assoc, SID, Routing),
       {ok, Ctx#{auth=>ok,authdata=>AuthData,state=>working}, State};
     Auth==false ->
       lager:debug("Auth fail"),
       gen_sctp:send(Socket, Assoc, SID, <<1>>),
       gen_sctp:eof(Socket, #sctp_assoc_change{
                               assoc_id = Assoc
                              }),
       {ok, Ctx#{auth=>fail,authdata=>AuthData,state=>authfail}, State}
  end;

payload(Socket,
        #sctp_sndrcvinfo{ stream=SID, assoc_id=Assoc }=Anc,
        Remote,Payload,#{state := working}=Ctx,#{handler:=TH}=State) when SID==0 ->
  case Payload of
    <<B0:8/integer,_/binary>> when
        (B0 >= 16#80 andalso 16#8f >= B0) orelse
        (B0 == 16#DE) ->
      case msgpack:unpack(Payload) of
        {ok, Unpacked} ->
          zerostream(Socket,Anc,Remote,Unpacked,Ctx,State);
        _ ->
          lager:notice("Can't decode MP in 0 stream, closing assoc"),
          {ok, close}
      end;
    <<MsgID:8/binary,HLen:8/integer,Rest/binary>> ->
      if HLen<128 ->
           <<Header:HLen/binary,Body/binary>> = Rest,
           Res= try
                  TH:handle_tpic(
                    {Assoc,SID,MsgID},
                    service,
                    Header,
                    Body,
                    Ctx)
                catch Ec:Ee ->
                        S=erlang:get_stacktrace(),
                        lager:error("Error in TPIC handler ~p:~p at ~p",
                                    [Ec,Ee,hd(S)]),
                        error
                end,
           lager:debug("call tpic handler (~p,~p,~p,~p,~p) -> ~p",
                       [{Assoc,SID,MsgID}, service, Header, Body, Ctx, Res]),
           case Res of
             ok ->
               {ok, State};
             close ->
               {ok, close};
             {ok, NewState} ->
               {ok, NewState};
             _Any ->
               {ok,State}
           end;
         true ->
           throw({"TPIC proto error",{HLen,">=128 reserved"}})
      end
  end;


payload(_Socket,
        #sctp_sndrcvinfo{ stream=SID, assoc_id=Assoc },
        {_RemoteIP,_RemotePort},Payload,Ctx,
        #{reqmap:=HQ,rrouting:=RR,handler:=TH}=State) ->
  case maps:is_key(SID,RR) of
    true ->
      {MsgID,Header,Body} = case Payload of
                              <<MsgIDm:8/binary,HLen:8/integer,Rest/binary>> ->
                                if HLen<128 ->
                                     <<Headerm:HLen/binary,Bodym/binary>> = Rest,
                                     {MsgIDm, Headerm, Bodym};
                                   true ->
                                     throw({"TPIC proto error",{HLen,">=128 reserved"}})
                                end;
                              _ ->
                                throw({"TPIC proto error","Can't match payload"})
                            end,
      Res=case hashqueue:get(MsgID,HQ) of
            X when is_pid(X) ->
              lager:debug("RM: msg ~p pid ~p",[MsgID, X]),
              try
                TH:handle_response(
                  {Assoc,SID,MsgID},
                  X,
                  Header,
                  Body,
                  Ctx)
              catch Ec:Ee ->
                      S=erlang:get_stacktrace(),
                      lager:error("Error in TPIC handler ~p:~p at ~p",
                                  [Ec,Ee,hd(S)]),
                      error
              end;
            undefined ->
              P=case maps:get(SID,RR) of
                  <<"timesync">> -> synchronizer;
                  X when is_pid(X) -> X;
                  X when is_atom(X) -> X;
                  X when is_binary(X) ->
                    try
                      erlang:binary_to_existing_atom(X,utf8)
                    catch error:badarg ->
                            undefined
                    end
                end,
              lager:debug("RM: msg ~p nopid (~p)",[MsgID,P]),
              XRes=try
                     TH:handle_tpic(
                       {Assoc,SID,MsgID},
                       P,
                       Header,
                       Body,
                       Ctx)
                   catch Ec:Ee ->
                           S=erlang:get_stacktrace(),
                           lager:error("Error in TPIC handler ~p:~p at ~p",
                                       [Ec,Ee,hd(S)]),
                           error
                   end,
              lager:debug("call tpic handler (~p,~p,~p,~p,~p) -> ~p",
                          [{Assoc,SID,MsgID}, service, Header, Body, Ctx, XRes]),
              XRes
          end,
      case Res of
        ok ->
          {ok, State};
        close ->
          {ok, close};
        {ok, NewState} ->
          {ok, NewState};
        _Any ->
          {ok,State}
      end;
    false ->
      lager:notice("Unknown payload from ~p ~p: ~p",
                   [Assoc, SID, Payload]),
      {ok, State}
  end.

zerostream(_Socket,
           #sctp_sndrcvinfo{ stream=SID, assoc_id=_Assoc }=_Anc,
           _Remote, #{
             null:=<<"routing">>,
             <<"routing">>:=Routes
            }=_Message,Ctx,State) when SID==0 ->
  {ok, Ctx#{routes=>Routes}, State};

zerostream(_Socket,
           #sctp_sndrcvinfo{ stream=SID, assoc_id=Assoc }=_Anc,
           _Remote, #{}=_Message,_Ctx,State) when SID==0 ->
  lager:debug("Message from ~p ~p", [Assoc, _Message]),
  {ok, State}.


clean_reqmap(HQ) ->
  case hashqueue:head(HQ) of
    empty ->
      HQ;
    T when is_integer(T) ->
      CT=erlang:system_time(second),
      if(CT-T<60) ->
          HQ;
        true ->
          {HQ1,_}=hashqueue:pop(HQ),
          clean_reqmap(HQ1)
      end
  end.

mkreqid(Node,Req) ->
  X=erlang:phash2(Node)+(Req bsr 32),
  <<X:32/big,Req:32/big>>.


init_address(Address, AddrState) ->
  case mkmap:get(Address, AddrState, not_found) of
    not_found ->
      mkmap:put(Address, #{}, AddrState);
    _ ->
      AddrState
  end.

resolve_addresses(init, Addresses) ->
  resolve_addresses(mkmap:new(), Addresses);


resolve_addresses(PeerInfo, Addresses) ->
  lists:foldl(
    fun({Host,Port},Acc) ->
        case resolve(Host) of
          [] ->
            Acc;
          [E1] ->
            init_address({E1,Port}, Acc);
          [E1|Rest] ->
            lists:foldl(
              fun(E2,Acc2) ->
                  mkmap:add_key({E1,Port},{E2,Port},Acc2)
              end, mkmap:put({E1,Port},#{},Acc),
              Rest
             )
        end
    end, PeerInfo, Addresses
   ).


add_peers(PeerInfo, NewPeers) ->
  resolve_addresses(PeerInfo, NewPeers).

encapmsg(MsgID, MBody) when is_binary(MBody) ->
  <<MsgID/binary,0,MBody/binary>>;

encapmsg(MsgID, {MHdr, MBody}) when is_binary(MHdr), is_binary(MBody) ->
  if size(MHdr) > 127 ->
       throw({error, longhdr});
     true ->
       <<MsgID/binary,
         (size(MHdr)):8/integer,
         MHdr/binary,
         MBody/binary>>
  end.

