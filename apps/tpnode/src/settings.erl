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

-module(settings).

-export([new/0, set/3, patch/2, mp/1, dmp/1, get/2]).
-export([sign/2, verify/1, verify/2, get_patches/1, get_patches/2]).
-export([make_meta/2, clean_meta/1]).

sign(Patch, PrivKey) when is_list(Patch) ->
    BinPatch=mp(Patch),
    sign(#{patch=>BinPatch, sig=>[]}, PrivKey);
sign(Patch, PrivKey) when is_binary(Patch) ->
    sign(#{patch=>Patch, sig=>[]}, PrivKey);

sign(#{patch:=LPatch}=Patch, PrivKey) ->
    BPatch=if is_list(LPatch) -> mp(LPatch);
                is_binary(LPatch) -> LPatch
             end,
    Sig=bsig:signhash(
          crypto:hash(sha256, BPatch),
          [{timestamp, os:system_time(millisecond)}],
          PrivKey),
    #{ patch=>BPatch,
        sig => [Sig|maps:get(sig, Patch, [])]
     }.

verify(#{patch:=LPatch, sig:=HSig}=Patch, VerFun) ->
  BinPatch=if is_list(LPatch) -> mp(LPatch);
              is_binary(LPatch) -> LPatch
           end,
  {Valid, Invalid}=bsig:checksig(crypto:hash(sha256, BinPatch), HSig),
  case length(Valid) of
    0 ->
      bad_sig;
    N when N>0 ->
      Map=lists:foldl(%make signatures unique
            fun(#{extra:=ED}=P,Acc) ->
                case proplists:get_value(pubkey,ED) of
                  PK when is_binary(PK) ->
                    maps:put(PK,P,Acc);
                  _ ->
                    Acc
                end
            end, #{}, Valid),
      ValidSig=if VerFun==undefined ->
                    maps:values(Map);
                 is_function(VerFun) ->
                    maps:fold(
                      fun(K,V,Acc) ->
                          case VerFun(K) of
                            true ->
                              [V|Acc];
                            false ->
                              Acc
                          end
                      end, [], Map)
               end,
      {ok, Patch#{
             sigverify=>#{
               valid=>ValidSig,
               invalid=>Invalid
              }
            }
      }
  end.

verify(#{patch:=_, sig:=_}=Patch) ->
  verify(Patch, undefined).

new() ->
    #{}.

set(A, B, C) ->
    change(set, A, B, C).

clean_meta(Set) when is_map(Set) ->
  S1=maps:remove(<<".">>,Set),
  maps:map(
    fun(_,V) when is_map(V)->
        clean_meta(V);
       (_,V) ->
        V
    end, S1).

meta_path(set, Path) ->
  {Pre,Post}=lists:split(length(Path)-1,Path),
  Pre++[<<".">>]++Post;

meta_path(list, Path) ->
  {Pre,Post}=lists:split(length(Path)-1,Path),
  Pre++[<<".">>]++Post.

make_meta1(Patch, MetaInfo, Acc) when is_binary(Patch) ->
  make_meta1(dmp(Patch), MetaInfo, Acc);

make_meta1([], _, Acc) ->
  Acc;

make_meta1([#{<<"t">>:=<<"list_",_/binary>>,<<"p">>:=Path,<<"v">>:=_}|Rest], MetaInfo, Acc) ->
  Acc2=maps:fold(
         fun(K,V,Acc1) ->
             BasePath=meta_path(list,Path),
             maps:put(BasePath++[K],V,Acc1)
         end, Acc, MetaInfo),
  make_meta1(Rest, MetaInfo, Acc2);

make_meta1([#{<<"t">>:=<<"set">>,<<"p">>:=Path,<<"v">>:=_}|Rest], MetaInfo, Acc) ->
  Acc2=maps:fold(
         fun(K,V,Acc1) ->
             BasePath=meta_path(set,Path),
             maps:put(BasePath++[K],V,Acc1)
         end, Acc, MetaInfo),
  make_meta1(Rest, MetaInfo, Acc2);

make_meta1([#{<<"t">>:=_,<<"p">>:=_,<<"v">>:=_}|Rest], MetaInfo, Acc) ->
  make_meta1(Rest, MetaInfo, Acc).

fixtype(X) when is_integer(X) -> X;
fixtype(X) when is_binary(X) -> X;
fixtype(X) when is_atom(X) -> atom_to_binary(X,utf8);
fixtype(X) ->
  throw({'bad_type',X}).

make_meta(Patches, MetaInfo) ->
    MI=maps:fold(
       fun(K,V,Acc) ->
           maps:put(fixtype(K),fixtype(V),Acc)
       end, #{}, MetaInfo),
  Map=make_meta1(Patches, MI, #{}),
  maps:fold(
    fun(Key,Val, Acc) ->
        [#{<<"t">>=><<"set">>, <<"p">>=>Key, <<"v">>=>Val}|Acc]
    end, [], Map).


get([], M) -> M;
get([Hd|Path], M) when is_list(Path) ->
    H1=maps:get(Hd, M, #{}),
    get(Path, H1).

change(Action, [Element|Path], Value, M, FPath) when
      Element==<<"chain">> orelse
      Element==<<"chains">> orelse
      Element==<<"nodechain">> orelse
      Element==<<"keys">> orelse
      Element==<<"globals">> orelse
      Element==<<"patchsig">> orelse
      Element==<<"blocktime">> orelse
      Element==<<"minsig">> orelse
      Element==<<"enable">> orelse
      Element==<<"params">> orelse
      Element==<<"disable">> orelse
      Element==<<"nodes">> ->
    change(Action, [binary_to_atom(Element, utf8)|Path], Value, M, FPath);

change(add, [], Value, M, FPath) ->
    M1=if M==#{} -> [];
          is_list(M) -> M;
          true -> throw({'non_list', FPath})
       end,
    lists:usort([Value|M1]);

change(remove, [], Value, M, FPath) ->
    M1=if M==#{} -> [];
          is_list(M) -> M;
          true -> throw({'non_list', FPath})
       end,
    lists:usort(M1--[Value]);

change({member, T}, [], Value, M, FPath) ->
    M1=if is_list(M) -> M;
          true -> throw({'non_list', FPath})
       end,
    case lists:member(Value, M1) of
        T ->
            M;
        _ ->
            throw({'member', FPath})
    end;

change({exist, T}, [Path], _Value, M, FPath) ->
    if is_map(M) ->
           Exist=maps:is_key(Path, M),
           if Exist == T ->
                  M;
              true ->
                  throw({exist, FPath})
           end;
       true ->
           throw({'non_map', FPath})
    end;


change(compare, [Path], Value, M, FPath) ->
    if is_map(M) ->
           Val=maps:get(Path, M, undefined),
           if Val==Value ->
                  M;
              true ->
                  throw({compare, FPath})
           end;
       true ->
           throw({'non_map', FPath})
    end;

change(delete, [Path], null, M, FPath) -> %force delete
    if is_map(M) ->
           maps:remove(Path, M);
       true ->
           throw({'non_map', FPath})
    end;

change(delete, [Path], Value, M, FPath) -> %compare and delete
    if is_map(M) ->
           Val=maps:get(Path, M, undefined),
           if Val==Value ->
                  maps:remove(Path, M);
              true ->
                  throw({delete_val, FPath, Value})
           end;
       true ->
           throw({'non_map', FPath})
    end;


change(set, [Path], Value, M, FPath) -> %set or replace
    if is_map(M) ->
           PrevValue=maps:get(Path, M, undefined),
           if is_list(PrevValue) orelse is_map(PrevValue) ->
                  throw({'non_value', FPath});
              true ->
                  maps:put(Path, Value, M)
           end;
       true ->
           throw({'non_map', FPath})
    end;

change(Action, [Hd|Path], Value, M, FPath) when is_list(Path) ->
    H1=maps:get(Hd, M, #{}),
    maps:put(Hd, change(Action, Path, Value, H1, FPath), M).

change(Action, Path, Value, M) when is_list(Path) ->
    change(Action, Path, Value, M, Path).

patch1([], M) -> M;

patch1([#{<<"t">>:=Action, <<"p">>:=K, <<"v">>:=V}|Settings], M) ->
    M1=change(action(Action), K, V, M),
    patch1(Settings, M1).

%txv2
patch({_TxID, #{patches:=Patch, sig:=Sigs}}, M) ->
    patch(#{patch=>Patch, sig=>Sigs}, M);

%tvx1
patch({_TxID, #{patch:=Patch, sig:=Sigs}}, M) ->
    patch(#{patch=>Patch, sig=>Sigs}, M);

%txv1
patch(#{patch:=Patch}, M) ->
    patch(Patch, M);

%naked
patch(Changes, M) when is_list(Changes) ->
  patch1(Changes, M);

%packed
patch(MP, M) when is_binary(MP) ->
    DMP=dmp(MP),
    patch1(DMP, M).

dmp(Term) when is_binary(Term) ->
    {ok, T}=msgpack:unpack(Term, [
                                %Not supported in msgpack or broken
                                %{known_atoms, [ chain ]}
                               ]),
    T;

dmp(Term) when is_list(Term) -> Term.

mp(Term) ->
    msgpack:pack(Term, [{map_format, jiffy}, {spec, new}]).
%mpk(Key) ->
%    E1=binary:split(Key, <<":">>, [global]),
%    mp(E1).

action(<<"list_add">>) -> add;
action(<<"list_del">>) -> remove;
action(<<"set">>) -> set;
action(<<"delete">>) -> delete;
action(<<"compare">>) -> compare;
action(<<"exist">>) -> {exist, true};
action(<<"nonexist">>) -> {exist, false};
action(<<"member">>) -> {member, true};
action(<<"nonmember">>) -> {member, false};
action(Action) -> throw({action, Action}).

get_patches(Settings) ->
  get_patches(Settings, export).

get_patches(Settings, Mode = export) when is_map(Settings) ->
    dmp(mp(lists:reverse(parse_settings(maps:keys(Settings), Settings, [], [], Mode))));

get_patches(Settings, Mode = ets) when is_map(Settings) ->
    lists:reverse(parse_settings(maps:keys(Settings), Settings, [], [], Mode)).

parse_settings([], _, _, Patches, _Mode) -> Patches;
parse_settings([H|T], Settings, Path, Patches, Mode) ->
  NewPath = [H|Path],
  Item = maps:get(H, Settings),
  NewPatches = case Item of
                 #{} ->
                   parse_settings(maps:keys(Item), Item, NewPath, Patches, Mode);
                 [_|_] ->
                   lists:foldl(
                     fun(Elem, Acc) ->
                         [#{<<"t">> => <<"list_add">>,
                            <<"p">> => lists:reverse(NewPath),
                            <<"v">> => Elem}|Acc]
                     end, Patches, Item);
                 _ when not is_map(Item), not is_list(Item) ->
                   [#{<<"t">> => <<"set">>,
                      <<"p">> => lists:reverse(NewPath),
                      <<"v">> => Item}|Patches]
               end,
  parse_settings(T, Settings, Path, NewPatches, Mode).
