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

-module(vm).
-export([run/4, test_erl/0, test_wasm/0]).

test_erl() ->
  SPid=vm_erltest:run("127.0.0.1",5555),
  timer:sleep(200),
  try
    Tx=tx:pack(
         tx:construct_tx(
           #{ver=>2,
             kind=>deploy,
             from=><<128,0,32,0,2,0,0,3>>,
             seq=>5,
             t=>1530106238743,
             payload=>[
                       #{amount=>10, cur=><<"XXX">>, purpose=>transfer },
                       #{amount=>20, cur=><<"FEE">>, purpose=>srcfee }
                      ],
             call=>#{function=>"init",args=>[48815]},
             txext=>#{"code"=>element(2,file:read_file("./examples/testcontract.ec"))}
            })
        ),
    run(fun(Pid) ->
            lager:info("Got worker ~p",[Pid]),
            Pid ! {run,
                   Tx,
                   msgpack:pack(#{}),
                   11111,
                   self()
                  },
            ok
        end, "erltest", 1, [])
  after
    SPid ! stop
  end.

test_wasm() ->
  {ok,Code}=file:read_file("./examples/testcontract.wasm"),
  Tx1=tx:pack(tx:construct_tx(
               #{ver=>2,
                 kind=>deploy,
                 from=><<128,0,32,0,2,0,0,3>>,
                 seq=>5,
                 t=>1530106238743,
                 payload=>[
                           #{amount=>10, cur=><<"XXX">>, purpose=>transfer },
                           #{amount=>20, cur=><<"FEE">>, purpose=>srcfee }
                          ],
                 call=>#{function=>"init",args=>[<<512:256/big>>]},
                 txext=>#{"code"=>Code}
                })),
  L0=msgpack:pack(
      #{
      %"code"=><<>>,
      "state"=>msgpack:pack(#{
%                 <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">> => <<"BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB">>,
%                 <<"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX">> => <<"YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY">>
                })
     }),
  {ok,#{"state":=S1}}=run(fun(Pid) ->
                              Pid ! {run, Tx1, L0, 11111, self() }
      end, "wasm", 2, []),

  Tx2=tx:pack(tx:construct_tx(
               #{ver=>2,
                 kind=>generic,
                 from=><<128,0,32,0,2,0,0,3>>,
                 to=><<128,0,32,0,2,0,0,3>>,
                 seq=>5,
                 t=>1530106238743,
                 payload=>[
                           #{amount=>10, cur=><<"XXX">>, purpose=>transfer },
                           #{amount=>20, cur=><<"FEE">>, purpose=>srcfee }
                          ],
                 call=>#{function=>"inc",args=>[<<1:256/big>>]}
                })),
  L1=msgpack:pack(
      #{
      "code"=>Code,
      "state"=>S1
      }),
  {ok,#{"state":=S2}=R2}=run(fun(Pid) ->
                      Pid ! {run, Tx2, L1, 11111, self() }
                  end, "wasm", 2, []),
%  {ok,UT2}=msgpack:unpack(Tx2),
  {msgpack:unpack(S1),
   msgpack:unpack(S2),
   R2
  }.

run(Fun, VmType, VmVer, _Opts) ->
  case gen_server:call(tpnode_vmsrv,{pick, VmType, VmVer, self()}) of
    {ok, Pid} ->
      Fun(Pid),
      R=receive {run_req, ReqNo} ->
                  receive {result, ResNo, Res} when ResNo == ReqNo ->
                            case Res of
                              pong ->
                                {ok, pong};
                              {ok, Payload, Ext} ->
                                lager:info("Contract ext ~p",[Ext]),
                                {ok,Payload};
                              {error, Err} ->
                                lager:error("Error ~p",[Err]),
                                {error, Err}
                            end
                  after 1000 ->
                          no_result
                  end
        after 5000 ->
                no_request
        end,
      gen_server:cast(tpnode_vmsrv,{return,Pid}),
      R;
    Any -> Any
  end.

