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

-module(mkblock_benchmark).
-export([benchmark/1]).

benchmark(N) ->
    Parent=crypto:hash(sha256, <<"123">>),
    Pvt1= <<194, 124, 65, 109, 233, 236, 108, 24, 50, 151, 189, 216, 23, 42, 215, 220, 24, 240,
      248, 115, 150, 54, 239, 58, 218, 221, 145, 246, 158, 15, 210, 165>>,
    Pub1=tpecdsa:calc_pub(Pvt1, false),
    From=address:pub2addr(0, Pub1),
    Coin= <<"FTT">>,
    Addresses=lists:map(
                fun(_) ->
                        address:pub2addr(0, crypto:strong_rand_bytes(16))
                end, lists:seq(1, N)),
    GetSettings=fun(mychain) -> 0;
                   (settings) ->
                        #{
                      chains => [0],
                      chain =>
                      #{0 => #{blocktime => 5, minsig => 2, <<"allowempty">> => 0} }
                     };
                   ({endless, Address, _Cur}) when Address==From->
                        true;
                   ({endless, _Address, _Cur}) ->
                        false;
                   (Other) ->
                        error({bad_setting, Other})
                end,
    GetAddr=fun({_Addr, Cur}) ->
                    #{amount => 54.0, cur => Cur,
                      lastblk => crypto:hash(sha256, <<"parent0">>),
                      seq => 0, t => 0};
               (_Addr) ->
                    #{<<"FTT">> =>
                      #{amount => 54.0, cur => <<"FTT">>,
                        lastblk => crypto:hash(sha256, <<"parent0">>),
                        seq => 0, t => 0}
                     }
            end,

    {_, _Res}=lists:foldl(fun(Address, {Seq, Acc}) ->
                                Tx=#{
                                  amount=>1,
                                  cur=>Coin,
                                  extradata=>jsx:encode(#{}),
                                  from=>From,
                                  to=>Address,
                                  seq=>Seq,
                                  timestamp=>os:system_time()
                                 },
                                NewTx=tx:unpack(tx:sign(Tx, Pvt1)),
                                {Seq+1,
                                 [{binary:encode_unsigned(10000+Seq), NewTx}|Acc]
                                }
                        end, {2, []}, Addresses),
    T1=erlang:system_time(),
  _=mkblock:generate_block( _Res,
            {1, Parent},
            GetSettings,
            GetAddr,
          []),

    T2=erlang:system_time(),
    (T2-T1)/1000000.


