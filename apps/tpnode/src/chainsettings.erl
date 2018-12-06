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

-module(chainsettings).

-export([get/2,get/3]).
-export([get_val/1,get_val/2]).
-export([get_setting/1,
         is_our_node/1,
         is_our_node/2,
         settings_to_ets/1,
         all/0, by_path/1]).

is_our_node(PubKey, Settings) ->
  KeyDB=maps:get(keys, Settings, #{}),
  NodeChain=maps:get(nodechain, Settings, #{}),
  ChainNodes0=maps:fold(
                fun(Name, XPubKey, Acc) ->
                    maps:put(XPubKey, Name, Acc)
                end, #{}, KeyDB),
  MyName=maps:get(nodekey:get_pub(), ChainNodes0, undefined),
  MyChain=maps:get(MyName, NodeChain, 0),
  ChainNodes=maps:filter(
               fun(_PubKey, Name) ->
                   maps:get(Name, NodeChain, 0) == MyChain
               end, ChainNodes0),
  maps:get(PubKey, ChainNodes, false).

is_our_node(PubKey) ->
  {ok, NMap} = chainsettings:get_setting(chainnodes),
  maps:get(PubKey, NMap, false).

get_setting(Named) ->
  case ets:lookup(blockchain,Named) of
    [{Named, Value}] ->
      {ok, Value};
    [] ->
      error
  end.

all() ->
  R=ets:match(blockchain,{'$1','_','$3','$2'}),
  lists:foldl(
    fun([Path,Val,Act],Acc) ->
        settings:patch([#{<<"t">>=>Act, <<"p">>=>Path, <<"v">>=>Val}], Acc)
    end,
    #{},
    R
   ).

by_path(GetPath) ->
  R=ets:match(blockchain,{GetPath++'$1','_','$3','$2'}),
  case R of
    [[[],Val,<<"set">>]] ->
      Val;
    Any ->
      lists:foldl(
        fun([Path,Val,Act],Acc) ->
            settings:patch([#{<<"t">>=>Act, <<"p">>=>Path, <<"v">>=>Val}], Acc)
        end,
        #{},
        Any
       )
  end.

settings_to_ets(NewSettings) ->
  Patches=settings:get_patches(NewSettings,ets),
  Ver=erlang:system_time(),
  SetApply=lists:map(fun(#{<<"p">>:=Path,<<"t">>:=Action,<<"v">>:=Value}) ->
                  lager:info("Path ~p:~p",[Path,Action]),
                  {Path, Ver, Action, Value}
              end,  Patches),
  %ets:match(blockchain,{[<<"current">>,<<"fee">>|'$1'],'_','$2'})
  %-- ets:fun2ms( fun({_,T,_}=M) when T < Ver -> M end)
  ets:insert(blockchain,SetApply),
  ets:select_delete(blockchain,
                    [{{'_','$1','_','_'},[{'<','$1',Ver}],[true]}]
                   ),

  KeyDB=maps:get(keys, NewSettings, #{}),
  NodeChain=maps:get(nodechain, NewSettings, #{}),
  PubKey=nodekey:get_pub(),
  ChainNodes0=maps:fold(
                fun(Name, XPubKey, Acc) ->
                    maps:put(XPubKey, Name, Acc)
                end, #{}, KeyDB),
  MyName=maps:get(PubKey, ChainNodes0, undefined),
  MyChain=maps:get(MyName, NodeChain, 0),
  ChainNodes=maps:filter(
               fun(_PubKey, Name) ->
                   maps:get(Name, NodeChain, 0) == MyChain
               end, ChainNodes0),
  lager:info("My name ~s chain ~p our chain nodes ~p", [MyName, MyChain, maps:values(ChainNodes)]),
  ets:insert(blockchain,[{myname,MyName},{chainnodes,ChainNodes},{mychain,MyChain}]),
  NewSettings.

get_val(Name) ->
  get_val(Name, undefined).

get_val(Name, Default) when Name==minsig; Name==patchsig ->
  Val=by_path([<<"current">>,chain,Name]),
  if is_integer(Val) -> Val;
     true ->
       case ets:lookup(blockchain,chainnodes) of
         [{chainnodes,Map}] ->
           lager:error("No ~s specified!!!!!",[Name]),
           (maps:size(Map) div 2)+1;
         _ ->
           Default
       end
  end;

get_val(Name,Default) ->
  Val=by_path([<<"current">>,chain,Name]),
  if is_integer(Val) -> Val;
     true -> Default
  end.

get(Key, Settings) ->
  get(Key, Settings, fun() ->
               {Chain, _}=gen_server:call(blockchain, last_block_height),
               true=is_integer(Chain),
               Chain
           end).


get(allowempty, Settings, GetChain) ->
  get(<<"allowempty">>, Settings, GetChain);

get(patchsig, Settings, GetChain) ->
  case settings:get([<<"current">>,chain,patchsig],Settings) of
    I when is_integer(I) ->
      I;
    _ ->
      case settings:get([<<"current">>,chain,minsig],Settings) of
        I when is_integer(I) ->
          I;
        _ ->
          try
            Chain=GetChain(),
            R=settings:get([chain,Chain,minsig],Settings),
            true=is_integer(R),
            R
          catch _:_ ->
                  3
          end
      end
  end;


get(Name, Sets, GetChain) ->
  MinSig=settings:get([<<"current">>,chain,Name], Sets),
  if is_integer(MinSig) -> MinSig;
     true ->
       MinSig_old=settings:get([chain,GetChain(),Name], Sets),
       if is_integer(MinSig_old) ->
            lager:info("Got setting ~p from deprecated place",[Name]),
            MinSig_old;
          true -> undefined
       end
  end.

