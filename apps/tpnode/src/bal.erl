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

-module(bal).

-export([
         new/0,
         fetch/5,
         get_cur/2,
         put_cur/3,
         get/2,
         put/3,
         mput/5,
         mput/6,
         pack/1,
         unpack/1,
         merge/2,
         changes/1
        ]).

-define(FIELDS,
        [t, seq, lastblk, pubkey, ld, usk, state, code, vm]
       ).

-type balfield() :: 'amount'|'t'|'seq'|'lastblk'|'pubkey'|'ld'|'usk'|'state'|'code'|'vm'.
-type sparsebal () :: #{'amount'=>map(), 
                  'changes'=>[balfield()],
                  'seq'=>integer(),
                  't'=>integer(),
                  'lastblk'=>binary(),
                  'pubkey'=>binary(),
                  'ld'=>integer(),
                  'usk'=>integer(),
                  'state'=>binary(),
                  'code'=>binary(),
                  'vm'=>binary(),
                  'ublk'=>binary() %external attr
                 }.

-type bal () :: #{'amount':=map(), 
                  'changes':=[balfield()],
                  'seq'=>integer(),
                  't'=>integer(),
                  'lastblk'=>binary(),
                  'pubkey'=>binary(),
                  'ld'=>integer(),
                  'usk'=>integer(),
                  'state'=>binary(),
                  'code'=>binary(),
                  'vm'=>binary(),
                  'ublk'=>binary() %external attr
                 }.

-spec new () -> bal().
new() ->
  #{amount=>#{}, changes=>[]}.

-spec changes (bal()) -> sparsebal().
changes(Bal) ->
  Changes=maps:get(changes, Bal, []),
  maps:with([amount|Changes], maps:remove(changes, Bal)).


-spec fetch (binary(), binary(), boolean(),
             bal(), fun()) -> bal().
fetch(Address, _Currency, _Header, Bal, FetchFun) ->
  %    FetchCur=not maps:is_key(Currency, Bal0),
  %    IsHdr=maps:is_key(seq, Bal0),
  %    if(Header and not IsHdr) ->
  case maps:is_key(seq, Bal) of
    true -> Bal;
    false ->
      FetchFun(Address)
  end.

-spec get_cur (binary(), bal()) -> integer().
get_cur(Currency, #{amount:=A}=_Bal) ->
  maps:get(Currency, A, 0).

-spec put_cur (binary(), integer(), bal()) -> bal().
put_cur(Currency, Value, #{amount:=A}=Bal) ->
  Bal#{
    amount => A#{ Currency => Value},
    changes=>[amount|maps:get(changes, Bal, [])]
   }.

-spec mput (Cur::binary(), Amount::integer(), Seq::non_neg_integer(),
            T::non_neg_integer(), Bal::bal(), UseSK::boolean()|'reset') -> bal().

mput(Cur, Amount, Seq, T, Bal) ->
  mput(Cur, Amount, Seq, T, Bal, false).

mput(Cur, Amount, Seq, 0, #{amount:=A}=Bal, false) when is_integer(Amount),
                                                        is_integer(Seq) ->
  Bal#{
    changes=>[amount, seq, t|maps:get(changes, Bal, [])],
    amount=>A#{Cur=>Amount},
    seq=>Seq
   };

mput(Cur, Amount, Seq, T, #{amount:=A}=Bal, false) when is_integer(Amount),
                                                        is_integer(Seq),
                                                        is_integer(T),
                                                        T > 1500000000000,
                                                        T < 15000000000000 ->
  Bal#{
    changes=>[amount, seq, t|maps:get(changes, Bal, [])],
    amount=>A#{Cur=>Amount},
    seq=>Seq,
    t=>T
   };

mput(Cur, Amount, Seq, T, #{amount:=A}=Bal, true) when is_integer(Amount),
                                                       is_integer(Seq),
                                                       is_integer(T),
                                                       T > 1500000000000,
                                                       T < 15000000000000 ->
  USK=maps:get(usk, Bal, 0),
  Bal#{
    changes=>[amount, seq, t, usk|maps:get(changes, Bal, [])],
    amount=>A#{Cur=>Amount},
    seq=>Seq,
    t=>T,
    usk=>USK+1
   };

mput(Cur, Amount, Seq, T, #{amount:=A}=Bal, reset) when is_integer(Amount),
                                                        is_integer(Seq),
                                                        is_integer(T),
                                                        T > 1500000000000,
                                                        T < 15000000000000 ->
  Bal#{
    changes=>[amount, seq, t, usk|maps:get(changes, Bal, [])],
    amount=>A#{Cur=>Amount},
    seq=>Seq,
    t=>T,
    usk=>1
   };

mput(_Cur, _Amount, _Seq, T, _Bal, _) when T < 1500000000000 orelse
                                           T > 15000000000000 ->
  throw('bad_timestamp_format').

-spec put (atom(), integer()|binary(), bal()) -> bal().
put(seq, V, Bal) when is_integer(V) ->
  Bal#{ seq=>V,
        changes=>[seq|maps:get(changes, Bal, [])]
      };

put(t, V, Bal) when is_integer(V),
                    V > 1500000000000,
                    V < 15000000000000  %only msec, not sec and not usec
                    ->
  Bal#{ t=>V,
        changes=>[t|maps:get(changes, Bal, [])]
      };
put(lastblk, V, Bal) when is_binary(V) ->
  Bal#{ lastblk=>V,
        changes=>[lastblk|maps:get(changes, Bal, [])]
      };
put(pubkey, V, Bal) when is_binary(V) ->
  Bal#{ pubkey=>V,
        changes=>[pubkey|maps:get(changes, Bal, [])]
      };
put(ld, V, Bal) when is_integer(V) ->
  Bal#{ ld=>V,
        changes=>[ld|maps:get(changes, Bal, [])]
      };
put(vm, V, Bal) when is_binary(V) ->
  Bal#{ vm=>V,
        changes=>[vm|maps:get(changes, Bal, [])]
      };
put(state, V, Bal) when is_binary(V) ->
  case maps:get(state, Bal, undefined) of
    OldState when OldState==V ->
      Bal;
    _ ->
      Bal#{ state=>V,
            changes=>[state|maps:get(changes, Bal, [])]
          }
  end;
put(code, V, Bal) when is_binary(V) ->
  case maps:get(code, Bal, undefined) of
    OldCode when OldCode==V ->
      Bal;
    _ ->
      Bal#{ code=>V,
            changes=>[code|maps:get(changes, Bal, [])]
          }
  end;
put(usk, V, Bal) when is_integer(V) ->
  Bal#{ usk=>V,
        changes=>[usk|maps:get(changes, Bal, [])]
      };
put(T, _, _) ->
  throw({"unsupported bal field", T}).


-spec get (atom(), bal()) -> integer()|binary()|undefined.
get(seq, Bal) ->    maps:get(seq, Bal, 0);
get(t, Bal) ->      maps:get(t, Bal, 0);
get(pubkey, Bal) -> maps:get(pubkey, Bal, <<>>);
get(ld, Bal) ->     maps:get(ld, Bal, 0);
get(usk, Bal) ->    maps:get(usk, Bal, 0);
get(vm, Bal) ->     maps:get(vm, Bal, undefined);
get(state, Bal) ->  maps:get(state, Bal, <<>>);
get(code, Bal) ->   maps:get(code, Bal, <<>>);
get(lastblk, Bal) ->maps:get(lastblk, Bal, <<0, 0, 0, 0, 0, 0, 0, 0>>);
get(T, _) ->      throw({"unsupported bal field", T}).

-spec pack (bal()) -> binary().
pack(#{
  amount:=Amount
 }=Bal) ->
  msgpack:pack(
    maps:put(
      amount, Amount,
      maps:with(?FIELDS, Bal)
     )
   ).

-spec unpack (binary()) -> bal().
unpack(Bal) ->
  case msgpack:unpack(Bal, [{known_atoms, [amount|?FIELDS]}]) of
    {ok, #{amount:=_}=Hash} ->
      maps:put(changes, [],
               maps:filter( fun(K, _) -> is_atom(K) end, Hash)
              );
    _ ->
      throw('ledger_unpack_error')
  end.

-spec merge(bal(), bal()) -> bal().
merge(Old, New) ->
  P1=maps:merge(
       Old,
       maps:with(?FIELDS, New)
      ),
  Bals=maps:merge(
         maps:get(amount, Old, #{}),
         maps:get(amount, New, #{})
        ),
  P1#{amount=>Bals}.

