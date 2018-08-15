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

-module(genesis).
-export([genesis/0, new/2, new/1, settings/0]).

genesis() ->
    {ok, [Genesis]}=file:consult(application:get_env(tpnode,genesis,"genesis.txt")),
    Genesis.

new(HPrivKey) ->
  PrivKeys=case HPrivKey of
             [E|_] when is_list(E) ->
               [ hex:parse(E1) || E1 <- HPrivKey];
             [<<_:32/binary>> |_] ->
               HPrivKey;
             [E|_] when is_binary(E) ->
               [ hex:parse(E1) || E1 <- HPrivKey];
             E1 when is_list(E1) ->
               [hex:parse(E1)];
             E1 when is_binary(E1) ->
               [hex:parse(E1)]
           end,
  Set0=case PrivKeys of 
         [_] ->
           settings();
         [_,_|_] ->
           settings(
             lists:map(
               fun(Priv) ->
                   Pub=tpecdsa:calc_pub(Priv,true),
                   <<Ni:8/binary,_/binary>>=nodekey:node_id(Pub),
                   {<<"node_",Ni/binary>>,Pub}
               end, PrivKeys)
            )
       end,
    new(HPrivKey, Set0).

new(HPrivKey, Set0) ->
  PrivKeys=case HPrivKey of
             [E|_] when is_list(E) ->
               [ hex:parse(E1) || E1 <- HPrivKey];
             [<<_:32/binary>> |_] ->
               HPrivKey;
             [E|_] when is_binary(E) ->
               [ hex:parse(E1) || E1 <- HPrivKey];
             E1 when is_list(E1) ->
               [hex:parse(E1)];
             E1 when is_binary(E1) ->
               [hex:parse(E1)]
           end,
  Patch=lists:foldl(
          fun(PrivKey, Acc) ->
              settings:sign(Acc, PrivKey)
          end, Set0, PrivKeys),
  Settings=[ { bin2hex:dbin2hex(crypto:hash(md5,settings:mp(Set0))), Patch } ],
  Blk0=block:mkblock(
         #{ parent=><<0, 0, 0, 0, 0, 0, 0, 0>>,
            height=>0,
            txs=>[],
            bals=>#{},
            settings=>Settings,
            sign=>[]
          }),
  Genesis=lists:foldl(
            fun(PrivKey, Acc) ->
                block:sign(
                  Acc,
                  [{timestamp, os:system_time(millisecond)}],
                  PrivKey)
            end, Blk0, PrivKeys),
  file:write_file("genesis.txt", io_lib:format("~p.~n", [Genesis])),
  {ok, Genesis}.

settings() ->
  settings(
    [
    {<<"nodeb1">>,base64:decode("AganOY4DcSMZ078U9tR9+p0PkwDzwnoKZH2SWl7Io9Xb")},
    {<<"nodeb2">>,base64:decode("AzHXdEk2GymQDUy30Q/uPefemnQloXGfAiWCpoywM7eq")},
    {<<"nodeb3">>,base64:decode("AujH2xsSnOCVJ5mtVy7MQPcCfNEEnKghX0P9V+E+Vfo/")}
    ]
   ).

settings(Keys) ->
  lists:foldl(
    fun({Name,Key},Acc) ->
        [ #{t=>set, p=>[keys,Name], v=>Key} | Acc]
    end, 
    [
     #{t=>set, p=>[<<"current">>,chain, patchsigs], v=>2},
     #{t=>set, p=>[<<"current">>,chain, minsig], v=>2},
     #{t=>set, p=>[<<"current">>,chain, blocktime], v=>2},
     #{t=>set, p=>[<<"current">>,chain, <<"allowempty">>], v=>0},
     #{t=>set, p=>[chains], v=>[1,2,3,4]},
     #{t=>set, p=>[nodechain], v=>
       lists:foldl(fun({NN,_},Acc) ->
                       maps:put(NN,4,Acc)
                   end, #{}, Keys)
      }
    ],
    Keys).

