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
%% @doc tpnode_vmproto
%% @end
%%%-------------------------------------------------------------------
-module(tpnode_vmproto).
-create_date("2018-08-15").

-behaviour(ranch_protocol).

-export([start_link/4, init/4, childspec/1, childspec/2, loop/1]).
-export([req/2, reply/3]).

-record(req,
        {owner,
         t1
        }).

childspec(Host, Port) ->
  [
   ranch:child_spec(
    {vm_listener,Port}, ranch_tcp,
    [{port, Port}, {max_connections, 128}, {ip, Host}],
    ?MODULE,
    []
   )
  ].


childspec(Port) ->
  [
   ranch:child_spec(
    {vm_listener,Port}, ranch_tcp,
    [{port, Port}, {max_connections, 128}],
    ?MODULE,
    []
   )
  ].

start_link(Ref, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
  {ok, Pid}.

init(Ref, Socket, Transport, _Opts) ->
  ok = ranch:accept_ack(Ref),
  inet:setopts(Socket, [{active, once},{packet,4}]),
%Transport:close(Socket),
  loop(#{socket=>Socket, transport=>Transport, myseq=>0, reqs=>#{}}).

loop(#{socket:=Socket, transport:=Transport, reqs:=Reqs}=State) ->
  receive
    {tcp, Socket, <<Seq:32/big,Data/binary>>} ->
      %lager:info("Got seq ~b payload ~p",[Seq,Data]),
      {ok,Payload}=msgpack:unpack(Data),
      S1=case Seq rem 2 of
        0 ->
          handle_req(Seq bsr 1, Payload, State);
        1 ->
          handle_res(Seq bsr 1, Payload, State)
      end,
      inet:setopts(Socket, [{active, once}]),
      ?MODULE:loop(S1);
    {tcp_closed, Socket} ->
      lager:info("Client gone"),
      maps:fold(
        fun(ReqID,#req{owner=Owner},_Acc) ->
            Owner ! {result, ReqID, {error, vm_gone}}
        end, 0, Reqs),
      Transport:close(Socket);
    {ping, From} ->
      From ! {run_req, 0},
      From ! {result, 0, pong},
      ?MODULE:loop(State);
    {run, Transaction, Ledger, Gas, From} ->
      Seq=maps:get(myseq, State, 0),
      S1=req(#{null=>"exec",
                "tx"=>Transaction,
                "ledger"=>Ledger,
                "gas"=>Gas}, State),
      From ! {run_req, Seq},
      lager:debug("run tx ~p",[Seq]),
      R=#req{owner=From,t1=erlang:system_time()},
      ?MODULE:loop(S1#{reqs=>maps:put(Seq,R,Reqs)});
    Any ->
      lager:info("unknown message ~p",[Any]),
      ?MODULE:loop(State)
  end.

handle_req(Seq, #{null:="hello"}=Request, State) ->
  lager:debug("Got seq ~b hello ~p",[Seq, Request]),
  reply(Seq, Request, State),
  ok=gen_server:call(tpnode_vmsrv,{register, self(), Request}),
  State;

handle_req(Seq, Request, State) ->
  lager:info("Got req seq ~b payload ~p",[Seq, Request]),
  State.

handle_res(Seq, Result, #{reqs:=Reqs}=State) ->
  case maps:find(Seq, Reqs) of
    error ->
      lager:info("Got res seq ~b payload ~p",[Seq, Result]),
      State;
    {ok, #req{owner=Pid, t1=T1}} ->
      lager:debug("Got res seq ~b payload ~p",[Seq, Result]),
      Pid ! {result, Seq, {ok, Result, #{t=>erlang:system_time()-T1}}},
      State#{reqs=>maps:remove(Seq, Reqs)}
  end. 

req(Payload, #{myseq:=MS}=State) ->
  Seq=MS bsl 1,
  send(Seq, Payload, State#{myseq=>MS+1}).

reply(Seq, Payload, State) ->
  send(Seq bor 1, Payload, State).

send(Seq, Payload, #{socket:=Socket, transport:=Transport}=State) when is_map(Payload) ->
  lager:debug("Sending seq ~b : ~p",[Seq, Payload]),
  Data=msgpack:pack(Payload),
  if is_binary(Data) ->
       F=lists:flatten(io_lib:format("log/vmproto_req_~w.bin",[Seq])),
       file:write_file(F,Data),
       ok;
     true ->
       lager:error("Can't encode ~p",[Payload]),
       throw('badarg')
  end,
  Transport:send(Socket, <<Seq:32/big,Data/binary>>),
  State.

