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

-module(txstorage).

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

-export([store_tx/3, get_tx/2, get_tx/1, get_state/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init_table(EtsTableName) ->
  Table =
    ets:new(
      EtsTableName,
      [named_table, protected, set, {read_concurrency, true}]
    ),
  lager:info("Table created: ~p", [Table]).

init(Args) ->
  EtsTableName = maps:get(ets_name, Args, ?MODULE),
  init_table(EtsTableName),
  {ok, #{
    expire_tick_ms => 1000 * maps:get(expire_check_sec, Args, 60*60), % default: 1 hour
    timer_expire => erlang:send_after(10*1000, self(), timer_expire), % first timer fires after 10 seconds
    ets_name => EtsTableName,
    ets_ttl_sec => maps:get(ets_ttl_sec, Args, 60*60)  % default: 1 hour
  }}.

handle_call(state, _From, State) ->
  {reply, State, State};

handle_call(get_table_name, _From, #{ets_name:=EtsName} = State) ->
  {reply, EtsName, State};

handle_call({get, TxId}, _From, #{ets_name:=EtsName} = State) ->
  lager:notice("Get tx ~p", [TxId]),
  {reply, get_tx(TxId, EtsName), State};

handle_call(_Request, _From, State) ->
  lager:notice("Unknown call ~p", [_Request]),
  {reply, ok, State}.

handle_cast(
  {tpic, FromPubKey, Peer, PayloadBin},
  #{ets_ttl_sec:=Ttl, ets_name:=EtsName} = State) ->
  
  lager:debug(
    "txstorage got txbatch from ~p payload ~p",
    [ FromPubKey, PayloadBin]
  ),
  try
%% Payload
%%    #{
%%      null => <<"mkblock">>,
%%      chain => MyChain,
%%      lbh => LBH,
%%      txbatch => BatchBin,
%%      batchid => BatchId
%%    }
    
    {BatchId, BatchBin} =
      case
        msgpack:unpack(
          PayloadBin,
          [
            {known_atoms, [txbatch, batchid] },
            {unpack_str, as_binary}
          ])
      of
        {ok, Payload} ->
          case Payload of
            #{
              null := <<"mkblock">>,
              txbatch := Bin,
              batchid := Id
             } ->
                {Id, Bin};
            _InvalidPayload ->
              lager:error(
                "txstorage got invalid transaction batch payload: ~p",
                [_InvalidPayload]
              ),
              throw(invalid_payload)
          end;
        _ ->
          lager:error("txstorage can't unpack msgpack: ~p", [ PayloadBin ])
      end,

    {BatchId, Txs} =
      case txsync:parse_batch(BatchBin) of
        {[], _} ->
          throw(empty_batch);
        {_, <<"">>} ->
          throw(empty_batch);
        Batch ->
          Batch
      end,
    ValidUntil = os:system_time(second) + Ttl,
    _TxIds = store_tx_batch(Txs, FromPubKey, EtsName, ValidUntil),
    tpic:cast(tpic, Peer, BatchId)
    
  catch
    Ec:Ee ->
      utils:print_error(
        "can't place transaction into storage",
        Ec, Ee, erlang:get_stacktrace()
      )
  end,
  {noreply, State};

handle_cast({store, Txs}, State) when is_list(Txs)->
  handle_cast({store, Txs, [], #{}}, State);

handle_cast({store, Txs, Nodes}, State) when is_list(Nodes) ->
  handle_cast({store, Txs, Nodes, #{}}, State);

handle_cast({store, Txs, Nodes, Options}, #{ets_ttl_sec:=Ttl, ets_name:=EtsName} = State)
  when is_list(Nodes), is_list(Txs), is_map(Options) ->
  
  lager:debug("Store txs ~p, options ~p", [ Txs, Options ]),

  try
    ValidUntil = os:system_time(second) + Ttl,
    TxIds = store_tx_batch(Txs, Nodes, EtsName, ValidUntil),
    ParseOptions =
      fun
        (#{push_queue := _}) when length(TxIds) > 0 ->
          lager:info("push ids to txqueue: ~p", [TxIds]),
          gen_server:cast(txqueue, {push, TxIds});
        (#{push_head_queue := _}) when length(TxIds) > 0 ->
          lager:info("push head ids to txqueue: ~p", [TxIds]),
          gen_server:cast(txqueue, {push_head, TxIds});
        (_Opt) ->
          lager:debug("txstorage EndOfOptions TxIds: ~p, options: ~p", [TxIds, _Opt]),
          ok
      end,
    ParseOptions(Options)

  catch
    Ec:Ee ->
      utils:print_error(
        "can't place transaction into storage",
        Ec, Ee, erlang:get_stacktrace()
      )
  end,
  {noreply, State};

handle_cast(_Msg, State) ->
  lager:notice("Unknown cast ~p", [_Msg]),
  {noreply, State}.

handle_info(timer_expire,
  #{ets_name:=EtsName, timer_expire:=Tmr, expire_tick_ms:=Delay} = State) ->
  
  catch erlang:cancel_timer(Tmr),
  lager:debug("remove expired records"),
  Now = os:system_time(second),
  ets:select_delete(
    EtsName,
    [{{'_', '_', '_', '$1'}, [{'<', '$1', Now}], [true]}]
  ),
  {noreply,
    State#{
      timer_expire => erlang:send_after(Delay, self(), timer_expire)
    }
  };

handle_info({store, TxId, Tx, Nodes}, State) ->
  lager:debug("store tx ~p", [TxId]),
  handle_cast({store, [{TxId, Tx}], Nodes}, State);


handle_info(_Info, State) ->
  lager:notice("Unknown info  ~p", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

store_tx({TxId, Tx, Nodes}, Table, ValidUntil) ->
%%  lager:info("store tx ~p to ets", [TxId]),
%%  io:format("store tx ~p to table ~p ~n", [TxId, Table]),

%%  TODO: vaildate transaction before store it
  ets:insert(Table, {TxId, Tx, Nodes, ValidUntil}),
  TxId;

store_tx(Invalid, _Table, _ValidUntil) ->
  lager:error("can't store invalid transaction: ~p", [Invalid]),
  error.

%% ------------------------------------------------------------------

store_tx_batch(Txs, FromPubKey, Table, ValidUntil) when is_binary(FromPubKey) ->
  store_tx_batch(Txs, FromPubKey, Table, ValidUntil, []);

store_tx_batch(Txs, Nodes, Table, ValidUntil) when is_list(Nodes) ->
  store_tx_batch(Txs, Nodes, Table, ValidUntil, []).

store_tx_batch([], _FromPubKey, _Table, _ValidUntil, StoredIds) ->
  StoredIds;

store_tx_batch([{TxId, Tx}|Rest], Nodes, Table, ValidUntil, StoredIds)
  when is_list(Nodes) ->
    NewStoredIds = StoredIds ++ [store_tx({TxId, Tx, Nodes}, Table, ValidUntil)],
    store_tx_batch(Rest, Nodes, Table, ValidUntil, NewStoredIds);

store_tx_batch([{TxId, Tx}|Rest], FromPubKey, Table, ValidUntil, StoredIds)
  when is_binary(FromPubKey) ->
    NewStoredIds = StoredIds ++ [store_tx({TxId, Tx, [FromPubKey]}, Table, ValidUntil)],
    store_tx_batch(Rest, FromPubKey, Table, ValidUntil, NewStoredIds).

%% ------------------------------------------------------------------

get_tx(TxId) ->
  get_tx(TxId, ?MODULE).

get_tx(TxId, Table) ->
  case ets:lookup(Table, TxId) of
    [{TxId, Tx, Nodes, _ValidUntil}] ->
      {ok, {TxId, Tx, Nodes}};
    [] ->
      error
  end.


%% ------------------------------------------------------------------

get_state() ->
  gen_server:call(?MODULE, state).

