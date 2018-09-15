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

-module(mkpatches).
-compile(export_all).
-compile(nowarn_export_all).

h2i(H,From,To,Secret) ->
  lists:map(
    fun(N) ->
        Code=scratchpad:geninvite(N,Secret),
        case crypto:hash(md5,Code) of
          H ->
            io:format("~s~n",[Code]);
          _ -> ok
        end
    end, lists:seq(From,To)).

invites_patch(PowDiff,From,To,Secret) ->
      [
       #{t=>set, p=>[<<"current">>, <<"register">>, <<"diff">>], v=>PowDiff},
       #{t=>set, p=>[<<"current">>, <<"register">>, <<"invite">>], v=>1},
       #{t=>set, p=>[<<"current">>, <<"register">>, <<"cleanpow">>], v=>1}
      ]++lists:map(
           fun(N) ->
               Code=scratchpad:geninvite(N,Secret),
               io:format("~s~n",[Code]),
               #{t=><<"list_add">>, p=>[<<"current">>, <<"register">>, <<"invites">>], 
                 v=>crypto:hash(md5,Code)
                }
           end, lists:seq(From,To)).

test_reg_invites() ->
  Patches=[
           #{t=>set, p=>[current, chain, minsig], v=>7},
           #{t=>set, p=>[current, chain, patchsig], v=>7},
           #{t=>set, p=>[current, chain, blocktime], v=>1},
           #{t=>set, p=>[current, chain, allowempty], v=>0},

           #{t=><<"nonexist">>, p=>[current, allocblock, last], v=>any},
           #{t=>set, p=>[current, allocblock, group], v=>10},
           #{t=>set, p=>[current, allocblock, block], v=>101},
           #{t=>set, p=>[current, allocblock, last], v=>0},

           #{t=>set, p=>[<<"current">>, <<"endless">>, 
                         naddress:construct_public(10,101,1),
                         <<"FEE">>], v=>true},
           #{t=>set, p=>[<<"current">>, <<"endless">>, 
                         naddress:construct_public(10,101,2),
                         <<"SK">>], v=>true},
           #{t=>set, p=>[current, fee, params, <<"feeaddr">>], v=>
             naddress:construct_public(10,101,1) },
           #{t=>set, p=>[current, fee, params, <<"notip">>], v=>1},
           #{t=>set, p=>[current, fee, <<"FEE">>, <<"base">>], v=>trunc(1.0e9)},
           #{t=>set, p=>[current, fee, <<"FEE">>, <<"baseextra">>], v=>128},
           #{t=>set, p=>[current, fee, <<"FEE">>, <<"kb">>], v=>trunc(1.0e9)},

           invites_patch(16,0,99,<<"1">>)
          ],
  Patch=scratchpad:sign_patch(
          settings:dmp(
            settings:mp(lists:flatten(Patches))),
          "configs/tnc10/tn101/c101*.config" ),
  Patch.

add_node_to_chain(Name, PubKey, ChainNo) when is_binary(Name) andalso 
                                              is_binary(PubKey) andalso
                                              is_integer(ChainNo) ->
  Patches=[
           #{t=>set, p=>[keys, Name], v=>PubKey},
           #{t=>set, p=>[nodechain, Name], v=>ChainNo}
          ],
  Patch=scratchpad:sign_patch(
          settings:dmp(
            settings:mp(lists:flatten(Patches))),
          "configs/tnc10/tn101/c101*.config" ),
  Patch.

test_rucoin() ->
  Patches=[
           #{t=>set, p=>[<<"current">>, <<"endless">>, 
                         <<128,1,64,0,1,0,0,95>>,
                         <<"RCL">>], v=>true}
          ],
  Patch=scratchpad:sign_patch(
          settings:dmp(
            settings:mp(lists:flatten(Patches))),
          "configs/c1n*.config" ),
  Patch.

test_el() ->
  Patches=[
%           #{t=>set, p=>[<<"current">>, <<"endless">>, 
%                         naddress:construct_public(10,101,2),
%                         <<"FEE">>], v=>true},
           #{t=>set, p=>[<<"current">>, <<"endless">>, 
                         naddress:construct_public(10,101,1),
                         <<"SK">>], v=>true}
          ],
  Patch=scratchpad:sign_patch(
          settings:dmp(
            settings:mp(lists:flatten(Patches))),
          "configs/tnc10/tn101/c101*.config" ),
  Patch.
