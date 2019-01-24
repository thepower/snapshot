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

-module(txqueue).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_lbh/1, get_state/0]).

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
  {ok,
    #{
      queue=>queue:new(),
      inprocess=>hashqueue:new()
    }
  }.

handle_call(state, _From, State) ->
  {reply, State, State};

handle_call(_Request, _From, State) ->
  lager:notice("Unknown call ~p", [_Request]),
  {reply, ok, State}.


handle_cast({push, TxIds}, #{queue:=Queue} = State) when is_list(TxIds) ->
%%  lager:debug("push ~p", [TxIds]),
  stout:log(txqueue_push, [ {ids, TxIds} ]),
  {noreply, State#{
    queue=>lists:foldl( fun queue:in/2, Queue, TxIds)
  }};


handle_cast({push_head, TxIds}, #{queue:=Queue} = State) when is_list(TxIds) ->
%%  lager:debug("push head ~p", [TxIds]),
  stout:log(txqueue_pushhead, [ {ids, TxIds} ]),
  {noreply, State#{
    queue=>lists:foldl( fun queue:in_r/2, Queue, TxIds)
  }};

handle_cast({done, Txs}, #{inprocess:=InProc0} = State) ->
  stout:log(txqueue_done, [ {result, true}, {ids, Txs} ]),
  InProc1 =
    lists:foldl(
      fun
        ({Tx, _}, Acc) ->
          lager:info("TX queue ext tx done ~p", [Tx]),
          hashqueue:remove(Tx, Acc);
        (Tx, Acc) ->
          lager:debug("TX queue tx done ~p", [Tx]),
          hashqueue:remove(Tx, Acc)
      end,
      InProc0,
      Txs),
  gen_server:cast(txstatus, {done, true, Txs}),
  gen_server:cast(tpnode_ws_dispatcher, {done, true, Txs}),
  {noreply,
    State#{
      inprocess => InProc1
    }};

handle_cast({failed, Txs}, #{inprocess:=InProc0} = State) ->
  stout:log(txqueue_done, [ {result, failed}, {ids, Txs} ]),
  InProc1 = lists:foldl(
    fun
      ({_, {overdue, Parent}}, Acc) ->
        lager:info("TX queue inbound block overdue ~p", [Parent]),
        hashqueue:remove(Parent, Acc);
      ({TxID, Reason}, Acc) ->
        lager:info("TX queue tx failed ~s ~p", [TxID, Reason]),
        hashqueue:remove(TxID, Acc)
    end,
    InProc0,
    Txs),
  gen_server:cast(txstatus, {done, false, Txs}),
  gen_server:cast(tpnode_ws_dispatcher, {done, false, Txs}),
  {noreply, State#{
    inprocess => InProc1
  }};


handle_cast({new_height, H}, State) ->
  {noreply, State#{height=>H}};

handle_cast(settings, State) ->
  {noreply, load_settings(State)};

handle_cast(prepare, #{mychain:=MyChain, inprocess:=InProc0, queue:=Queue} = State) ->
  {Queue1, TxIds} =
    txpool:pullx({txpool:get_max_pop_tx(), txpool:get_max_tx_size()}, Queue, []),
  
  stout:log(txqueue_prepare, [ {ids, TxIds}, {node, nodekey:node_name()} ]),
  
  PK =
    case maps:get(pubkey, State, undefined) of
      undefined -> nodekey:get_pub();
      FoundKey -> FoundKey
    end,
  
  try
    PreSig = maps:merge(
      gen_server:call(blockchain, lastsig),
      #{null=><<"mkblock">>,
        chain=>MyChain
      }),
    MResX = msgpack:pack(PreSig),
    gen_server:cast(mkblock, {tpic, PK, MResX}),
    tpic:cast(tpic, <<"mkblock">>, MResX),
    stout:log(txqueue_xsig, [ {ids, TxIds} ])
  catch
    Ec:Ee ->
      utils:print_error("Can't send xsig", Ec, Ee, erlang:get_stacktrace())
  end,
  
  try
    LBH = get_lbh(State),
    TxMap =
      lists:foldl(
        fun(Id, Acc) -> maps:put(Id, null, Acc) end,
        #{},
        TxIds
      ),
    lager:debug("txs for mkblock: ~p", [TxMap]),
    MRes = msgpack:pack(
      #{
        null=><<"mkblock">>,
        chain=>MyChain,
        lbh=>LBH,
        txs=>TxMap
      }
    ),
    gen_server:cast(mkblock, {tpic, PK, MRes}),
    tpic:cast(tpic, <<"mkblock">>, MRes),
    stout:log(txqueue_mkblock, [{ids, TxIds}, {lbh, LBH}])
  catch
    Ec1:Ee1 ->
      utils:print_error("Can't encode", Ec1, Ee1, erlang:get_stacktrace())
  end,
  
  Time = erlang:system_time(seconds),
  {InProc1, Queue2} = recovery_lost(InProc0, Queue1, Time),
  ETime = Time + 20,
  
  {noreply,
    State#{
      queue=>Queue2,
      inprocess=>lists:foldl(
        fun(TxId, Acc) ->
          hashqueue:add(TxId, ETime, null, Acc)
        end,
        InProc1,
        TxIds
      )
    }};

handle_cast(prepare, State) ->
  lager:notice("TXQUEUE Blocktime, but I am not ready"),
  {noreply, load_settings(State)};


handle_cast(_Msg, State) ->
  lager:notice("Unknown cast ~p", [_Msg]),
  {noreply, State}.

%%handle_info(getlb, State) ->
%%  {_Chain,Height}=gen_server:call(blockchain,last_block_height),
%%  {noreply, State#{height=>Height}};

handle_info(prepare, State) ->
  handle_cast(prepare, State);

handle_info({push, TxIds}, State) when is_list(TxIds) ->
  handle_cast({push, TxIds}, State);

handle_info({push_head, TxIds}, State) when is_list(TxIds) ->
  handle_cast({push_head, TxIds}, State);
  
handle_info(_Info, State) ->
  lager:notice("Unknown info ~p", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


recovery_lost(InProc, Queue, Now) ->
  case hashqueue:head(InProc) of
    empty ->
      {InProc, Queue};
    I when is_integer(I) andalso I >= Now ->
      {InProc, Queue};
    I when is_integer(I) ->
      case hashqueue:pop(InProc) of
        {InProc1, empty} ->
          {InProc1, Queue};
        {InProc1, {TxID, _TxBody}} ->
          recovery_lost(InProc1, queue:in(TxID, Queue), Now)
      end
  end.

%% ------------------------------------------------------------------

load_settings(State) ->
  MyChain = blockchain:chain(),
  {_Chain, Height} = gen_server:call(blockchain, last_block_height),
  State#{
    mychain => MyChain,
    height => Height
  }.

%% ------------------------------------------------------------------

get_lbh(State) ->
  case maps:find(height, State) of
    error ->
      {_Chain, H1} = gen_server:call(blockchain, last_block_height),
      gen_server:cast(self(), {new_height, H1}), % renew height cache in state
      H1;
    {ok, H1} ->
      H1
  end.

%% ------------------------------------------------------------------

get_state() ->
  gen_server:call(?MODULE, state).

