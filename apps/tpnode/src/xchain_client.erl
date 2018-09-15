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

% -*- mode: erlang -*-
% vi: set ft=erlang :

-module(xchain_client).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Options) ->
  Name = maps:get(name, Options, xchain_client),
  gen_server:start_link({local, Name}, ?MODULE, Options, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  State = #{
    subs => init_subscribes(#{}),
    chain => blockchain:chain(),
    connect_timer => erlang:send_after(3 * 1000, self(), make_connections)
   },
  code:ensure_loaded(xchain_client_handler),
  {ok, State}.

handle_call(state, _From, State) ->
  {reply, State, State};

handle_call({add_subscribe, Subscribe}, _From, #{subs:=Subs} = State) ->
  AS=add_sub(Subscribe, Subs),
  lager:notice("xchain client add subscribe ~p: ~p", [Subscribe, AS]),
  {reply, ok, State#{
                subs => AS
               }};

handle_call(peers, _From, #{subs:=Subs} = State) ->
  {reply, get_peers(Subs), State};

handle_call(_Request, _From, State) ->
  lager:notice("xchain client unknown call ~p", [_Request]),
  {reply, ok, State}.

handle_cast(settings, State) ->
  lager:notice("xchain client reload settings"),
  {noreply, change_settings_handler(State)};

handle_cast({discovery, Announce, AnnounceBin}, #{subs:=Subs} = State) ->
  lager:notice(
    "xchain client got announce from discovery. " ++
    "Relay it to all active xchain connections."),
  try
    relay_discovery(Announce, AnnounceBin, Subs)
  catch
    Err:Reason ->
      lager:error(
        "xchain client can't relay announce ~p ~p ~p",
        [Err, Reason, Announce]
       )
  end,
  {noreply, State};

handle_cast(_Msg, State) ->
  lager:error("xchain client unknown cast ~p", [_Msg]),
  {noreply, State}.

handle_info({wrk_down, ConnPid, Reason}, #{subs:=Subs} = State) ->
  lager:notice("xchain client got close from server for pid ~p: ~p",
               [ConnPid, Reason]),
  {noreply, State#{
              subs => update_sub(
                        fun(_,Sub) ->
                            maps:remove(worker,Sub)
                        end, ConnPid, Subs)
             }};

handle_info(make_connections, #{connect_timer:=Timer, subs:=Subs} = State) ->
  catch erlang:cancel_timer(Timer),
  NewSubs = make_connections(Subs),
  {noreply, State#{
              subs => NewSubs,
              connect_timer => erlang:send_after(10 * 1000, self(), make_connections)
             }};

handle_info(_Info, State) ->
  lager:error("xchain client unknown info ~p", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
make_connections(Subs) ->
  maps:map(
    fun(_Key, Sub) ->
        case maps:is_key(worker, Sub) of
          false ->
            try
              lager:info("xchain client make connection to ~p",[Sub]),
              {ok, Pid} = xchain_client_worker:start_link(Sub),
              Sub#{worker=>Pid}
            catch
              Err:Reason ->
                lager:info("xchain client got error while connection to remote xchain: ~p ~p",
                           [Err, Reason]),
                Sub
            end;
          _ ->
            Sub
        end
    end,
    Subs
   ).

add_sub(#{address:=IP,port:=Port}=Subscribe, Subs) ->
  try
    Key = {IP,Port},
    NewSub = maps:merge(
               Subscribe,
               maps:get(Key, Subs, #{})
              ),
    maps:put(Key, NewSub, Subs)
  catch
    Reason ->
      lager:error("xchain client can't process subscribe. ~p ~p", [Reason, Subscribe]),
      Subs
  end.

get_peers(Subs) ->
  Parser =
  fun(_PeerKey, #{channels:=Channels, node_id:=NodeId, ws_mode:=true} = _PeerInfo, Acc) ->
      maps:put(NodeId, maps:keys(Channels), Acc);

     (_PeerKey, _PeerInfo, Acc) ->
      Acc
  end,
  maps:fold(Parser, #{}, Subs).

relay_discovery(_Announce, AnnounceBin, Subs) ->
  Sender =
  fun(_Key, #{worker:=W}, Cnt) ->
      W ! {send_msg, #{null=><<"xdiscovery">>, <<"bin">>=>AnnounceBin}},
      Cnt+1;
     (_Key, Sub, Cnt) ->
      lager:debug("Skip relaying to unfinished connection: ~p", [Sub]),
      Cnt
  end,
  Sent = maps:fold(Sender, 0, Subs),
  lager:debug("~p xchain discovery announces were sent", [Sent]),
  ok.

change_settings_handler(#{chain:=Chain, subs:=Subs} = State) ->
  CurrentChain = blockchain:chain(),
  case CurrentChain of
    Chain ->
      State;
    _ ->
      lager:info("xchain client wiped out all crosschain subscribes"),

      % close all active connections
      maps:fold(
        fun(_Key, #{worker:=W}=_Sub, Acc) ->
            W ! stop,
            Acc+1;
           (_Key, _Sub, Acc) ->
            Acc
        end,
        0,
        Subs),

      % and finally replace all subscribes by new ones
      State#{
        subs => init_subscribes(#{}),
        chain => CurrentChain
       }
  end.

init_subscribes(Subs) ->
  Config = application:get_env(tpnode, crosschain, #{}),
  ConnectIpsList = maps:get(connect, Config, []),
  lists:foldl(
    fun({Ip, Port}, Acc) when is_integer(Port) ->
        Sub = #{
          address => Ip,
          port => Port
         },
        add_sub(Sub, Acc);
       (Invalid, Acc) ->
        lager:error("xhcain client got invalid crosschain connect term: ~p", Invalid),
        Acc
    end, Subs, ConnectIpsList).

update_sub(Fun, GunPid, Subs) ->
  case find_sub_by_pid_and_ref(GunPid, Subs) of
    undefined ->
      Subs;
    {Matched,Found} ->
      Sub=maps:get(Found, Subs),
      Sub2=Fun(Matched, Sub),
      if(Sub==Sub2) ->
          Subs;
        true ->
          maps:put(Found, Sub2, Subs)
      end
  end.

find_sub_by_pid_and_ref(GunPid, Subs) ->
  maps:fold(
    fun(Key, #{worker:=Pid}, undefined) when Pid==GunPid ->
        {pid,Key};
       (_,_, Found) ->
        Found
    end, undefined, Subs).

