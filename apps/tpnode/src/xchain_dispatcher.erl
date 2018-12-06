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

-module(xchain_dispatcher).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, pub/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

pub(Channel, Payload) when is_binary(Channel) ->
    gen_server:call(xchain_dispatcher, {publish, Channel, Payload}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    {
        ok,
        #{
            pid_info => #{},
            pid_subs => #{},
            chan_subs => #{}
        }
    }.


handle_call(peers, _From, #{pid_info:=PidInfo} = State) ->
    {reply, get_peers(PidInfo), State};


handle_call(state, _From, State) ->
    {reply, State, State};


handle_call({publish, Channel, Data}, _From, State) ->
    SentCount = publish(Channel, Data, State),
    {reply, SentCount, State};


handle_call(_Request, _From, State) ->
    lager:error("xchain dispatcher got unknown call ~p", [_Request]),
    {reply, ok, State}.


handle_cast({register_peer, Pid, RemoteNodeId, RemoteChannels}, State) ->
    {noreply, register_peer({Pid, RemoteNodeId, RemoteChannels}, State)};


handle_cast({subscribe, Channel, Pid}, #{pid_subs:=_Pids, chan_subs:=_Chans}=State) ->
    {noreply, add_subscription(Channel, Pid, State)};

handle_cast(_Msg, State) ->
    lager:error("xchain dispatcher got unknown cast ~p", [_Msg]),
    {noreply, State}.


handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    {noreply, unsubscribe_all(Pid, State)};

handle_info(_Info, State) ->
    lager:error("xchain dispatcher got unknown info  ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


get_peers(PidInfo) ->
  Parser = fun(_ConnPid, PeerInfo, Acc) ->
               maps:merge(Acc, PeerInfo)
           end,
  try
    maps:fold(Parser, #{}, PidInfo)
  catch
    Ec:Ee ->
      iolist_to_binary(io_lib:format("~p:~p", [Ec, Ee]))
  end.

register_peer({Pid, RemoteNodeId, RemoteChannels}, #{pid_info:=PidInfo} = State) ->
    lager:info("xchain dispatcher register remote node ~p ~p ~p", [Pid, RemoteNodeId, RemoteChannels]),
    RemoteInfo = #{ RemoteNodeId => RemoteChannels},
    State#{
        pid_info => maps:put(Pid, RemoteInfo, PidInfo)
    }.

add_subscription(Channel, Pid, #{pid_subs:=Pids, chan_subs:=Chans}=State) ->
    lager:info("xchain dispatcher subscribe ~p to ~p", [Pid, Channel]),
    monitor(process, Pid),
    OldPidChannels = maps:get(Pid, Pids, #{}),
    NewPidChannels = maps:put(Channel, 1, OldPidChannels),
    OldChanPids = maps:get(Channel, Chans, #{}),
    NewChanPids = maps:put(Pid, 1, OldChanPids),

    State#{
        pid_subs => maps:put(Pid, NewPidChannels, Pids),
        chan_subs => maps:put(Channel, NewChanPids, Chans)
    }.


unsubscribe_all(Pid, #{pid_info:=PidInfo, pid_subs:=Pids, chan_subs:=Chans}=State)
    when is_pid(Pid) ->
    lager:info("xchain dispatcher remove all subs for pid ~p", [Pid]),
    State#{
        pid_info => maps:remove(Pid, PidInfo),
        pid_subs => maps:remove(Pid, Pids),
        chan_subs => maps:map(
            fun(_Chan, ChanPids) ->
                maps:without([Pid], ChanPids)
            end, Chans)
    }.

publish(Channel, Data, #{chan_subs:=Chans}=_State) ->
    Subscribers = maps:get(Channel, Chans, #{}),
    Publisher =
        fun(ClientPid, _SubscribeState, Counter) ->
            erlang:send(ClientPid, {message, Data}),
            Counter+1
        end,
    maps:fold(Publisher, 0, Subscribers).

