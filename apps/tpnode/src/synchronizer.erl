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

-module(synchronizer).
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
    gen_server:cast(self(), settings),
    {ok, #{
       myoffset=>undefined,
       offsets=>#{},
       timer5=>erlang:send_after(5000, self(), selftimer5),
       ticktimer=>erlang:send_after(6000, self(), ticktimer),
       tickms=>10000,
       prevtick=>0
      }}.

handle_call(peers, _From, #{offsets:=Offs}=State) ->
    Friends=maps:keys(Offs),
    {reply, Friends, State};

handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(settings, State) ->
    {noreply, load_settings(State)};

handle_cast({tpic, _PeerID, <<16#be, _/binary>>=Payload}, State) ->
	try
		Beacon=beacon:check(Payload),
		lager:info("SYNC beacon ~p", [Beacon]),
		{noreply, State}
	catch _:_ ->
			  {noreply, State}
	end;

handle_cast({tpic, _PeerID, Payload}, State) ->
    case msgpack:unpack(Payload) of
        {ok, #{null:=<<"hello">>, <<"n">>:=Node, <<"t">>:=T}} ->
            handle_cast({hello, Node, T}, State);
        Any ->
            lager:info("Bad TPIC received ~p", [Any]),
            {noreply, State}
    end;


handle_cast({hello, PID, WallClock}, State) ->
    Behind=erlang:system_time(microsecond)-WallClock,
    lager:debug("Hello from ~p our clock diff ~p", [PID, Behind]),
    {noreply, State#{
                offsets=>maps:put(PID,
                                  {
                                   Behind,
                                   erlang:system_time(seconds)
                                  },
                                  maps:get(offsets, State, #{})
                                 )
               }
    };

handle_cast({setdelay, Ms}, State) when Ms>900 ->
    lager:info("Setting ~p ms block delay", [Ms]),
    {noreply, State#{
                tickms=>Ms
               }
    };

handle_cast(_Msg, State) ->
    lager:info("Unknown cast ~p", [_Msg]),
    {noreply, State}.

handle_info(ticktimer,
            #{meandiff:=MeanDiff, ticktimer:=Tmr, tickms:=Delay, prevtick:=_T0}=State) ->
    T=erlang:system_time(microsecond),
    MeanMs=round((T+MeanDiff)/1000),
    Wait=Delay-(MeanMs rem Delay),
    case maps:get(bcready, State, false) of
        true ->
            gen_server:cast(txpool, prepare),
            erlang:send_after(200, whereis(mkblock), process),
            lager:info("Time to tick. next in ~w", [Wait]);
        false ->
            erlang:send_after(200, whereis(mkblock), flush),
            lager:info("Time to tick. But we not in sync. wait ~w", [Wait])
    end,
    catch erlang:cancel_timer(Tmr),

    {noreply, State#{
               ticktimer=>erlang:send_after(Wait, self(), ticktimer),
               prevtick=>T
              }
    };

handle_info(selftimer5, #{mychain:=_MyChain, tickms:=Ms, timer5:=Tmr, offsets:=Offs}=State) ->
    Friends=maps:keys(Offs),
    {Avg, Off2}=lists:foldl(
          fun(Friend, {Acc, NewOff}) ->
                  case maps:get(Friend, Offs, undefined) of
                      undefined ->
                          {Acc, NewOff};
                      {LOffset, LTime} ->
                          {[LOffset|Acc], maps:put(Friend, {LOffset, LTime}, NewOff)}
                  end
          end, {[], #{}}, Friends),
    MeanDiff=median(Avg),
    T=erlang:system_time(microsecond),
    Hello=msgpack:pack(#{null=><<"hello">>,
						 <<"n">>=>nodekey:node_id(),
						 <<"t">>=>T
						}),
    tpic:cast(tpic, <<"timesync">>, Hello),
    BCReady=try
                gen_server:call(blockchain, ready, 50)
            catch Ec:Ee ->
                      lager:error("SYNC BC is not ready err ~p:~p ", [Ec, Ee]),
                      false
            end,
    MeanMs=round((T-MeanDiff)/1000),
    if(Friends==[]) ->
          lager:debug("I'm alone in universe my time ~w", [(MeanMs rem 3600000)/1000]);
      true ->
          lager:info("I have ~b friends, and mean hospital time ~w, mean diff ~w blocktime ~w",
                     [length(Friends), (MeanMs rem 3600000)/1000, MeanDiff/1000, Ms]
                    )
    end,

    catch erlang:cancel_timer(Tmr),

    {noreply, State#{
               timer5=>erlang:send_after(10000-(MeanMs rem 10000)+500, self(), selftimer5),
               offsets=>Off2,
               meandiff=>MeanDiff,
               bcready=>BCReady
              }
    };

handle_info(_Info, State) ->
    lager:info("Unknown info ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
median([]) -> 0;
median([E]) -> E;
median(List) ->
    LL=length(List),
    DropL=(LL div 2)-1,
    {_, [M1, M2|_]}=lists:split(DropL, List),
    case LL rem 2 of
        0 -> %even elements
            (M1+M2)/2;
        1 -> %odd
            M2
    end.


load_settings(State) ->
    BlockTime=blockchain:get_mysettings(blocktime),
    MyChain=blockchain:get_mysettings(chain),
	BCReady=try
                gen_server:call(blockchain, ready, 50)
            catch Ec:Ee ->
                      lager:error("SYNC BC is not ready err ~p:~p ", [Ec, Ee]),
                      false
            end,
    State#{
      tickms=>BlockTime*1000,
      mychain=>MyChain,
      bcready=>BCReady
     }.


