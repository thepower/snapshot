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

-module(vm_erltest).
-export([run/2,client/2,loop/1]).

run(Host, Port) ->
  spawn(?MODULE,client,[Host, Port]).

client(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [{packet,4},binary]),
  inet:setopts(Socket, [{active, once}]),
  State=#{myseq=>0,
          transport=>gen_tcp,
          socket=>Socket
         },
  S1=tpnode_vmproto:req(#{null => "hello","lang" => "erltest","ver" => 1}, State),
  loop(S1).

loop(#{socket:=Socket, transport:=Transport}=State) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    stop ->
      Transport:close(Socket);
    {tcp, Socket, <<Seq:32/big,Data/binary>>} ->
      {ok,Payload}=msgpack:unpack(Data),
      S1=case Seq rem 2 of
           0 ->
             handle_req(Seq, Payload, State);
           1 ->
             handle_res(Seq bsr 1, Payload, State)
         end,
      ?MODULE:loop(S1)
  after 60000 ->
          ?MODULE:loop(State)
  end.

handle_res(Seq, Payload, State) ->
  lager:info("Res ~b ~p",[Seq,Payload]),
  State.

handle_req(Seq, #{null:="exec",
                  "gas":=Gas,
                  "ledger":=Ledger,
                  "tx":=BTx}, State) ->
  T1=erlang:system_time(),
  Tx=tx:unpack(BTx),
  Code=case maps:get(kind, Tx) of
         deploy ->
           maps:get("code",maps:get(txext,Tx));
         generic ->
           maps:get(<<"code">>,Ledger)
       end,
  %lager:info("Req ~b ~p",[Seq,maps:remove(body,Tx)]),
  Bindings=maps:fold(
             fun erl_eval:add_binding/3,
             erl_eval:new_bindings(),
             #{
               'Gas'=>Gas,
               'Ledger'=>Ledger,
               'Tx'=>Tx
              }
            ),
  try
  T2=erlang:system_time(),
  Ret=eval(Code, Bindings),
  T3=erlang:system_time(),
  lager:debug("Ret ~p",[Ret]),
  case Ret of
    {ok, RetVal, NewState, NewGas, NewTxs} ->
      T4=erlang:system_time(),
      tpnode_vmproto:reply(Seq, #{
                             null => "exec",
                             "gas" => NewGas,
                             "ret" => RetVal,
                             "state" => NewState,
                             "txs" => NewTxs,
                             "dt"=>[T2-T1,T3-T2,T4-T3]
                            },State);
    _ ->
      T4=erlang:system_time(),
      tpnode_vmproto:reply(Seq,
                           #{
                             null => "exec",
                             "dt"=> [T2-T1,T3-T2,T4-T3],
                             "gas" => Gas,
                             "ret" => "error"
                            },
                           State)
  end
  catch Ec:Ee ->
          S=erlang:get_stacktrace(),
          lager:error("Error ~p:~p", [Ec, Ee]),
          lists:foreach(fun(SE) ->
                            lager:error("@ ~p", [SE])
                        end, S),

          tpnode_vmproto:reply(Seq,
                               #{
                                 null => "exec",
                                 "error"=> iolist_to_binary(
                                             io_lib:format("crashed ~p:~p",[Ec,Ee])
                                            )
                                },
                               State)
  end;

handle_req(Seq, Payload, State) ->
  lager:info("Req ~b ~p",[Seq,Payload]),
  State.



eval(Source, Bindings) ->
  SourceStr = binary_to_list(Source),
  {ok, Tokens, _} = erl_scan:string(SourceStr),
  {ok, Parsed} = erl_parse:parse_exprs(Tokens),
  case erl_eval:exprs(Parsed, Bindings) of
    {value, Result, _} -> Result;
    Any ->
      lager:error("Error ~p",[Any]),
      throw('eval_error')
  end.

