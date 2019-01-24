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

% beacon announce
handle_tpic(From, mkblock, <<"beacon">>, Beacon, _State) ->
  lager:debug("Beacon ~p", [Beacon]),
  gen_server:cast(topology, {got_beacon, From, Beacon}),
  ok;

% relayed beacon announce
handle_tpic(From, mkblock, <<"beacon2">>, Beacon, _State) ->
  lager:debug("Beacon2 ~p", [Beacon]),
  gen_server:cast(topology, {got_beacon2, From, Beacon}),
  ok;

handle_tpic(From, mkblock, <<"txbatch">>, Payload, #{authdata:=AD}=_State) ->
  lager:debug("txbatch: form ~p payload ~p", [ From, Payload ]),
  gen_server:cast(txstorage, {tpic, proplists:get_value(pubkey, AD), From, Payload}),
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

handle_tpic(From, blockchain, <<"chainkeeper">>, Payload, #{authdata:=AD}=_State) ->
  NodeKey = proplists:get_value(pubkey, AD),
  NodeName = chainsettings:is_our_node(NodeKey),
  lager:debug("Got chainkeeper beacon From ~p p ~p", [From, Payload]),
  gen_server:cast(chainkeeper, {tpic, NodeName, From, Payload}),
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

