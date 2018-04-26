-module(tpnode_tpic_handler).
-behaviour(tpic_handler).
-export([init/1, handle_tpic/5, routing/1, handle_response/5]).

init(_S) ->
  lager:info("TPIC Init ~p", [_S]),
  {ok, #{}}.

routing(_State) ->
  #{ <<"timesync">>=>synchronizer,
     <<"mkblock">>=>mkblock,
     <<"blockvote">>=>blockvote,
     <<"blockchain">>=>blockchain
   }.

handle_tpic(From, _, <<"tping">>, Payload, _State) ->
  lager:debug("tping"),
  Rnd=rand:uniform(300),
  timer:sleep(Rnd),
  lager:info("TPIC tping ~p", [_State]),
  tpic:cast(tpic, From, <<"delay ", (integer_to_binary(Rnd))/binary,
                          " pong ", Payload/binary, " from ",
                          (atom_to_binary(node(), utf8))/binary>>),
  ok;

handle_tpic(From, mkblock, <<"beacon">>, Beacon, _State) ->
  lager:info("Beacon ~p", [Beacon]),
  gen_server:cast(topology, {tpic, From, Beacon}),
  ok;

handle_tpic(_From, mkblock, <<>>, Payload, #{authdata:=AD}=_State) ->
  lager:debug("mkblock from ~p payload ~p",[_From,Payload]),
  gen_server:cast(mkblock, {tpic, proplists:get_value(pubkey, AD), Payload}),
  ok;

handle_tpic(_From, service, <<"discovery">>, Payload, _State) ->
  lager:debug("Service discovery from ~p payload ~p", [_From,Payload]),
  gen_server:cast(discovery, {got_announce, Payload}),
  ok;

handle_tpic(_From, service, Hdr, Payload, _State) ->
  lager:info("Service from ~p hdr ~p payload ~p", [_From, Hdr, Payload]),
  ok;

handle_tpic(From, _To, <<"kickme">>, Payload, State) ->
  lager:info("TPIC kick HANDLER from ~p ~p ~p", [From, _To, Payload, State]),
  close;

handle_tpic(From, blockchain, <<"ping">>, Payload, _State) ->
  tpic:cast(tpic, From, <<"pong ", Payload/binary, " from ",
                          (atom_to_binary(node(), utf8))/binary>>),
  ok;

handle_tpic(From, blockchain, <<"ledger">>, Payload, _State) ->
  lager:info("Ledger TPIC From ~p p ~p", [From, Payload]),
  ledger:tpic(From, Payload),
  ok;

handle_tpic(From, To, <<>>, Payload, _State) when To==synchronizer orelse
                                                  To==blockvote orelse
                                                  To==mkblock orelse
                                                  To==blockchain ->
  lager:debug("Generic TPIC to ~p from ~p payload ~p", [To,From,Payload]),
  gen_server:cast(To, {tpic, From, Payload}),
  ok;

handle_tpic(From, To, Header, Payload, _State) ->
  lager:info("unknown TPIC ~p from ~p ~p ~p", [To, From, Header, Payload]),
  ok.

handle_response(From, To, _Header, Payload, State) ->
  lager:debug("TPIC resp HANDLER from ~p to ~p: ~p ~p ~p", [From, To, _Header, Payload, State]),
  gen_server:cast(To, {tpic, From, Payload}),
  ok.

