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

-module(txsync).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, do_sync/2, make_batch/1, parse_batch/1]).

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
    tx_storage => #{}
  }}.

handle_call(_Request, _From, State) ->
  lager:notice("Unknown call ~p", [_Request]),
  {reply, ok, State}.


handle_cast({new_tx, _TxId, _TxBody}, State) ->
  
  {noreply, State};
  
handle_cast(_Msg, State) ->
  lager:notice("Unknown cast ~p", [_Msg]),
  {noreply, State}.

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

do_sync([], _Options) ->
  lager:info("txsync: skip empty transactions list"),
  ok;

do_sync(Transactions, _Options) when is_list(Transactions) ->
  try
    lager:info("txsync: start for ~p", [Transactions]),
    
    {BatchId, BatchBin} =
      case make_batch(Transactions) of
        {<<"">>, _} ->
          throw(empty_batch);
        {_, <<"">>} ->
          throw(empty_batch);
        {_, _} = TransactionBatch ->
          TransactionBatch
      end,
      
    Peers = tpic:cast_prepare(tpic, <<"mkblock">>),
    
%%    lager:debug("tpic peers: ~p", [Peers]),
    
    MRes = msgpack:pack(
      #{
        null => <<"mkblock">>,
        txbatch => BatchBin,
        batchid => BatchId
      }
    ),
    
    % Unconfirmed = [ #{ tpic_handle => pub_key } ]
    Unconfirmed =
      lists:foldl(
        fun
          ({TpicHandle, #{authdata:=AD}}, Acc) ->
            case proplists:get_value(pubkey, AD, undefined) of
              undefined ->
                Acc;
              PeerPubKey ->
                maps:put(TpicHandle, PeerPubKey, Acc)
            end;
          (_, Acc) ->
            Acc
        end,
        #{},
        Peers),
    tpic:cast(tpic, <<"mkblock">>, {<<"txbatch">>, MRes}),
    wait_response(
      #{
        unconfirmed => Unconfirmed,
        confirmed => #{},
        conf_timeout_ms => get_wait_confs_timeout_ms(),
        batchid => BatchId,
        txs => maps:from_list(Transactions)
      }
    ),
    ok
  catch
    Ec:Ee ->
      utils:print_error("do_sync error", Ec, Ee, erlang:get_stacktrace()),
      error
  end.

%% ------------------------------------------------------------------
wait_response(
  #{unconfirmed := Unconfirmed,
    confirmed := Confirmed,
    conf_timeout_ms := TimeoutMs,
    batchid := BatchId,
    txs := TxMap} = State) ->
  
  receive
%% {'$gen_cast',{tpic,{61,4,<<5,102,134,118,0,0,5,193>>},<<"fake_tx_id">>}}
    {'$gen_cast', {tpic, From, BatchId}}  ->
      Handle = get_tpic_handle(From),
      Confirmed1 =
        case maps:get(Handle, Unconfirmed, unknown) of
          unknown ->
            Confirmed; % don't touch confirmations
          PubKey ->
            lager:info("got confirmation from ~p", [PubKey]),
            maps:put(PubKey, 1, Confirmed)
        end,
      Unconfirmed1 = maps:remove(Handle, Unconfirmed),
      case maps:size(Unconfirmed1) of
        0 ->
          % all confirmations received
          lager:info("got all confirmations for ~p", [BatchId]),
          store_batch(TxMap, Confirmed, #{push_queue => true}),
          ok;
        _ ->
          wait_response(
            State#{
              unconfirmed => Unconfirmed1,
              confirmed => Confirmed1
            }
          )
      end;
    Any ->
      lager:error("unhandled message: ~p", [Any]),
      wait_response(State)
    after TimeoutMs ->
      % confirmations waiting cycle timeout
      store_batch(TxMap, Confirmed, #{push_queue => true}),
      lager:error("EOF for batch ~p", [BatchId])
  end,
  ok.


%% ------------------------------------------------------------------
% Txs = [ #{ TxId => TxBody } ]
% Nodes = #{ PubKey => 1 }
%%store_batch(Txs, Nodes) ->
%%  store_batch(Txs, Nodes, #{}).

store_batch(Txs, Nodes, Options) ->
  gen_server:cast(txstorage, {store, maps:to_list(Txs), maps:keys(Nodes), Options}).


%% ------------------------------------------------------------------
%% {61,4,<<5,102,134,118,0,0,5,193>>} -> {61,4}
get_tpic_handle({A, B, _}) ->
  {A, B}.

%% ------------------------------------------------------------------
get_wait_confs_timeout_ms() ->
  chainsettings:get_val(<<"conf_timeout">>, 2000).

%% ------------------------------------------------------------------
make_batch(Transactions) when is_list(Transactions) ->
  make_batch(Transactions, <<"">>, <<"">>).

make_batch([], Batch, BatchId) ->
  {BatchId, Batch};

make_batch([{TxId, TxBody} | Rest], Batch, _BatchId) ->
  make_batch(
    Rest,
    <<Batch/binary,
      (size(TxId)):8/big,
      (size(TxBody)):32/big,
      TxId/binary, TxBody/binary>>,
    TxId   % we use the last transaction id as batch id
  ).

%% ------------------------------------------------------------------

parse_batch(Batch) when is_binary(Batch) ->
  parse_batch(Batch, [], <<"">>).

parse_batch(<<"">>, ParsedTxs, BatchId) ->
  {BatchId, ParsedTxs};

parse_batch(
  <<S1:8/big, S2:32/big, TxId:S1/binary, TxBody:S2/binary, Rest/binary>>,
  Parsed,
  _BatchId) ->
  
  parse_batch(Rest, Parsed ++ [{TxId, TxBody}], TxId).


