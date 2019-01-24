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

%%%-------------------------------------------------------------------
%% @doc tpnode_vmsrv gen_server
%% @end
%%%-------------------------------------------------------------------
-module(tpnode_vmsrv).

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
  {ok, #{
     vmtype=>#{},
     vmbytype=>#{},
     monitor=>#{},
     inuse=>#{}
    }
  }.

handle_call({pick, Lang, Ver, CPid}, _From, 
            #{vmbytype:=VmByType,
              monitor:=Monitor
             }=State) ->
  Type={Lang,Ver},
  case maps:get(Type, VmByType, []) of
    [] ->
      {reply, {error, noworkers}, State};
    Workers ->
      {NA,Worker}=lists:foldr(fun({_WP,#{usedby:=_}}=R,{Acc,undefined}) -> 
                            {[R|Acc], undefined };
                           ({_WP,#{}}=R,{Acc,undefined}) ->
                            {Acc, R};
                           (R,{Acc,Found}) ->
                            { [R|Acc], Found }
                        end, {[],undefined}, Workers),
      case Worker of
        undefined ->
          {reply, {error, nofree}, State};
        {Pid, Params} ->
          lager:debug("Picked worker ~p",[Worker]),
          MR=erlang:monitor(process, CPid),
          Monitor2=maps:put(MR, {user, Pid}, Monitor),
          VmByType2=maps:put(Type, [{Pid,Params#{usedby=>CPid, mref=>MR}}|NA], VmByType),
          {reply, {ok, Pid}, State#{
                               vmbytype=>VmByType2,
                               monitor=>Monitor2
                              }
          }
      end
  end;

handle_call({register, Pid, #{
                         null:="hello", 
                         "lang":=Lang, 
                         "ver":=Ver
                        }}, _From,
            #{monitor:=Monitor,
              vmtype:=VmType,
              vmbytype:=VmByType}=State) ->
  Type={Lang,Ver},
  MR=erlang:monitor(process, Pid),
  link(Pid),
  Monitor2=maps:put(MR, {worker, Type}, Monitor),
  VmByType2=maps:put(Type,[{Pid,#{}}|maps:get(Type,VmByType,[])],VmByType),
  lager:debug("Reg ~p ~p",[Pid,Type]),
  {reply, ok, State#{
                vmbytype=>VmByType2,
                vmtype=>maps:put(Pid,Type,VmType),
                monitor=>Monitor2
               }};

handle_call(_Request, _From, State) ->
    lager:notice("Unknown call ~p",[_Request]),
    {reply, ok, State}.

handle_cast({return, Pid}, State) ->
  {noreply, return_worker(Pid, State)};

handle_cast(_Msg, State) ->
    lager:notice("Unknown cast ~p",[_Msg]),
    {noreply, State}.

handle_info({'DOWN',Ref,process,Pid,_Reason}, 
            #{monitor:=Monitor,
              vmtype:=VmType,
             vmbytype:=VmByType}=State) ->
  case maps:find(Ref, Monitor) of 
    error ->
      lager:notice("Down for unknown ref ~p",[Ref]),
      {noreply, State};
    {ok, {user, WPid}} ->
      lager:debug("Down user",[Pid]),
      %Monitor2=maps:remove(Ref, Monitor),
      S1=return_worker(WPid, State),
      {noreply, S1};
    {ok, {worker, Type}} ->
      Monitor2=maps:remove(Ref, Monitor),
      Workers1=maps:get(Type,VmByType,[]),
      Workers=lists:keydelete(Pid,1,Workers1),
      VmByType2=if Workers==[] ->
                     maps:remove(Type, VmByType);
                   true ->
                     maps:put(Type,Workers,VmByType)
                end,
      lager:debug("UnReg ~p ~p",[Pid,Type]),
      {noreply, State#{
                  vmbytype=>VmByType2,
                  vmtype=>maps:remove(Pid,VmType),
                  monitor=>Monitor2
                 }}
  end;

handle_info(_Info, State) ->
    lager:notice("Unknown info  ~p",[_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

return_worker(WPid, #{ vmtype:=VmType,
                       monitor:=Mon,
                       vmbytype:=VmByType}=State) ->
  case maps:find(WPid, VmType) of
    {ok, Type} ->
      Workers=maps:get(Type,VmByType,[]),
      {WPid, WState}=lists:keyfind(WPid,1,Workers),
      Mon1=case maps:get(mref,WState,undefined) of
        undefined -> Mon;
        Ref ->
          erlang:demonitor(Ref),
          maps:remove(Ref, Mon)
      end,
      Workers2=lists:keydelete(WPid,1,Workers),
      VmByType2=maps:put(Type,
                         [{WPid, maps:without([usedby,mref],WState)}|Workers2],
                         VmByType),
      State#{
        vmbytype=>VmByType2,
        monitor=>Mon1
       };
    error ->
      State
  end.
