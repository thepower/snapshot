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

-module(txstatus).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(CLEANUP, 30000).
-define(TIMEOUT, 300).

-ifndef(TEST).
-define(TEST, 1).
-endif.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,get/1,get_json/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

get(TxID) ->
  gen_server:call(?MODULE, {get, TxID}).

get_json(TxID) ->
  R=gen_server:call(?MODULE, {get, TxID}),
  if R==undefined ->
       null;
     true ->
       jsonfy(R)
  end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, #{ q=>hashqueue:new(),
          timer=>erlang:send_after(?CLEANUP, self(), timer)
        }
  }.

handle_call({get, TxID}, _From, #{q:=Q}=State) ->
  R=hashqueue:get(TxID, Q),
  {reply, R, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({done, Result, Txs}, #{q:=Q}=State) when is_list(Txs)->
  %{done,false,[{<<"1524179A464B33A2-3NBx74EdmT2PyYexBSrg7xcht998-03A2">>,{contract_error, [error,{badmatch,#{<<"fee">> => 30000000,<<"feecur">> => <<"FTT">>,<<"message">> => <<"To AA100000001677722185 with love">>}}]}}]}
  %{done,true,[<<"AA1000000016777220390000000000000009xQzCH+qGbhzKlrFxoZOLWN5DhVE=">>]}
  Timeout=erlang:system_time(seconds)+?TIMEOUT,
  Q1=lists:foldl(
       fun({TxID,Res},QAcc) ->
           hashqueue:add(TxID, Timeout, {Result, Res}, QAcc);
          (TxID,QAcc) ->
           hashqueue:add(TxID, Timeout, 
                         {Result, 
                         if Result -> 
                              ok;
                            true -> error 
                         end}, QAcc)
       end, Q, Txs),
  {noreply, 
   State#{q=>Q1}
  };


handle_cast(_Msg, State) ->
  lager:notice("Unhandler cast ~p",[_Msg]),
  {noreply, State}.

handle_info(timer, #{timer:=Tmr}=State) ->
  catch erlang:cancel_timer(Tmr),
  handle_info({cleanup, 
               erlang:system_time(seconds)}, 
              State#{
                timer=>erlang:send_after(?CLEANUP, self(), timer)
               });

handle_info({cleanup, Time}, #{q:=Queue}=State) ->
  Q1=cleanup(Queue,Time),
  {noreply, State#{q=>Q1}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

jsonfy({false,{error,{contract_error,[Ec,Ee]}}}) ->
  #{ error=>true,
     res=><<"smartcontract">>,
     type=>Ec,
     details=>iolist_to_binary(io_lib:format("~p",[Ee]))};

jsonfy({true,#{address:=Addr}}) ->
  #{ok=>true,
    res=>naddress:encode(Addr)
   };

jsonfy({true,Status}) ->
  #{ok=>true,
    res=> iolist_to_binary(io_lib:format("~p",[Status]))
   };

jsonfy({false,Status}) ->
  #{error=>true,
    res=>iolist_to_binary(io_lib:format("~p",[Status]))
   }.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

cleanup(Queue, Now) ->
  case hashqueue:head(Queue) of
    empty ->
      Queue;
    I when is_integer(I) andalso I>=Now ->
      Queue;
    I when is_integer(I) ->
      case hashqueue:pop(Queue) of
        {Queue1, empty} ->
          Queue1;
        {Queue1, _} ->
          cleanup(Queue1, Now)
      end
  end.



-ifdef(TEST).
txstatus_test() ->
    {ok, Pid}=?MODULE:start_link(txstatus_test),
    gen_server:cast(Pid,{done, true, [<<"1234">>,<<"1235">>]}),
    gen_server:cast(Pid,{done, false, [{<<"1334">>,"err1"},
                                       {<<"1335">>,"err2"}
                                      ]}),
    timer:sleep(2000),
    gen_server:cast(Pid,{done, false, [<<"1236">>,<<"1237">>]}),
    S1=[
        ?assertMatch({true,ok}, gen_server:call(Pid,{get, <<"1234">>})),
        ?assertMatch({true,ok}, gen_server:call(Pid,{get, <<"1235">>})),
        ?assertMatch({false,"err1"}, gen_server:call(Pid,{get, <<"1334">>})),
        ?assertMatch({false,"err2"}, gen_server:call(Pid,{get, <<"1335">>})),
        ?assertMatch({false,error}, gen_server:call(Pid,{get, <<"1236">>})),
        ?assertMatch({false,error}, gen_server:call(Pid,{get, <<"1237">>}))
       ],
    Pid ! {cleanup, erlang:system_time(seconds)+?TIMEOUT-1},
    S2=[
        ?assertMatch(undefined, gen_server:call(Pid,{get, <<"1234">>})),
        ?assertMatch(undefined, gen_server:call(Pid,{get, <<"1235">>})),
        ?assertMatch({false,error}, gen_server:call(Pid,{get, <<"1236">>})),
        ?assertMatch({false,error}, gen_server:call(Pid,{get, <<"1237">>}))
       ],
    Pid ! {cleanup, erlang:system_time(seconds)+?TIMEOUT+1},
    S3=[
        ?assertMatch(undefined, gen_server:call(Pid,{get, <<"1234">>})),
        ?assertMatch(undefined, gen_server:call(Pid,{get, <<"1235">>})),
        ?assertMatch(undefined, gen_server:call(Pid,{get, <<"1236">>})),
        ?assertMatch(undefined, gen_server:call(Pid,{get, <<"1237">>}))
       ],
    gen_server:stop(Pid, normal, 3000),
    S1++S2++S3.
-endif.

