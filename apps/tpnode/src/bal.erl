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

-spec new () -> #{amount:=map()}.
new() ->
	#{amount=>#{}, changes=>[]}.

-spec changes (map()) -> map().
changes(Bal) ->
	Changes=maps:get(changes, Bal, []),
	maps:with([amount|Changes], maps:remove(changes, Bal)).


-spec fetch (integer()|binary(), integer()|binary(), 'true'|'false',
			 map(), fun()) -> map().
fetch(Address, _Currency, _Header, Bal, FetchFun) ->
	%    FetchCur=not maps:is_key(Currency, Bal0),
	%    IsHdr=maps:is_key(seq, Bal0),
	%    if(Header and not IsHdr) ->
	case maps:is_key(seq, Bal) of
		true -> Bal;
		false ->
			FetchFun(Address)
	end.

-spec get_cur (integer()|binary(), map()) -> integer().
get_cur(Currency, #{amount:=A}=_Bal) ->
	maps:get(Currency, A, 0).

-spec put_cur (integer()|binary(), integer(), #{amount:=map()}) -> map().
put_cur(Currency, Value, #{amount:=A}=Bal) ->
	Bal#{
	  amount => A#{ Currency => Value},
	  changes=>[amount|maps:get(changes, Bal, [])]
	 }.

-spec mput (integer()|binary(), integer(), integer(), integer(), map()) -> map().
mput(Cur, Amount, Seq, T, Bal) ->
	mput(Cur, Amount, Seq, T, Bal, false).

-spec mput (integer()|binary(), integer(), integer(), integer(), map(), atom()) -> map().
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

-spec put (atom(), integer()|binary(), map()) -> map().
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


-spec get (atom(), map()) -> integer()|binary().
get(seq, Bal) ->		maps:get(seq, Bal, 0);
get(t, Bal) ->			maps:get(t, Bal, 0);
get(pubkey, Bal) ->		maps:get(pubkey, Bal, <<>>);
get(ld, Bal) ->			maps:get(ld, Bal, 0);
get(usk, Bal) ->		maps:get(usk, Bal, 0);
get(vm, Bal) ->			maps:get(vm, Bal, undefined);
get(state, Bal) ->		maps:get(state, Bal, <<>>);
get(code, Bal) ->		maps:get(code, Bal, <<>>);
get(lastblk, Bal) ->	maps:get(lastblk, Bal, <<0, 0, 0, 0, 0, 0, 0, 0>>);
get(T, _) ->			throw({"unsupported bal field", T}).

-spec pack (map()) -> binary().
pack(#{
  amount:=Amount
 }=Bal) ->
	msgpack:pack(
	  maps:put(
		amount, Amount,
		maps:with(?FIELDS, Bal)
	   )
	 ).

-spec unpack (binary()) -> map().
unpack(Bal) ->
	case msgpack:unpack(Bal, [{known_atoms, [amount|?FIELDS]}]) of
		{ok, #{amount:=_}=Hash} ->
			maps:put(changes, [],
					 maps:filter( fun(K, _) -> is_atom(K) end, Hash)
					);
		_ ->
			throw('ledger_unpack_error')
	end.

-spec merge(map(), map()) -> map().
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

