-module(scratchpad).
-compile(export_all).

node_id() ->
    {ok,K1}=application:get_env(tpnode,privkey),
    address:pub2addr(node,secp256k1:secp256k1_ec_pubkey_create(hex:parse(K1), true)).

sign(Message) ->
    {ok,PKeyH}=application:get_env(tpnode,privkey),
    PKey=hex:parse(PKeyH),
    Msg32 = crypto:hash(sha256, Message),
    Sig = secp256k1:secp256k1_ecdsa_sign(Msg32, PKey, default, <<>>),
    Pub=secp256k1:secp256k1_ec_pubkey_create(PKey, true),
    <<Pub/binary,Sig/binary,Message/binary>>.

verify(<<Public:33/binary,Sig:71/binary,Message/binary>>) ->
    {ok,TrustedKeys}=application:get_env(tpnode,trusted_keys),
    Found=lists:foldl(
      fun(_,true) -> true;
         (HexKey,false) ->
              Pub=hex:parse(HexKey),
              Pub == Public
      end,false, TrustedKeys),
    Msg32 = crypto:hash(sha256, Message),
    {Found,secp256k1:secp256k1_ecdsa_verify(Msg32, Sig, Public)}.

parsekey(<<"0x",BKey/binary>>) ->
    hex:parse(BKey);
parsekey(Base58) ->
    B58Decode=base58:decode(Base58),
    KS=size(B58Decode)-5,
    case B58Decode of
        <<128,KeyBody:KS/binary,KC:4/binary>> ->
            <<H3:4/binary,_/binary>>=
            crypto:hash(sha256,
                        crypto:hash(sha256,<<128:8/integer,KeyBody/binary>>)
                       ),
            if(KC==H3) ->
                  KeyBody;
              true ->
                  error
            end;
        _ ->
            error
    end.


gentx(BFrom,To,Amount,HPrivKey) when is_binary(BFrom)->
    From=binary_to_list(BFrom),
    Cur= <<"FTT">>,
    inets:start(),
    {ok,{{_HTTP11,200,_OK},_Headers,Body}}=
    httpc:request(get,{"http://127.0.0.1:43280/api/address/"++From,[]},[],[{body_format,binary}]),
    #{<<"info">>:=C}=jsx:decode(Body,[return_maps]),
    #{<<"seq">>:=Seq}=maps:get(Cur,C,#{<<"amount">> => 0,<<"seq">> => 0}),
    NewTx=tx:sign(#{
      amount=>Amount,
      cur=>Cur,
      extradata=>jsx:encode(#{
                   message=><<"preved from gentx">>
                  }),
      from=>BFrom,
      to=>To,
      seq=>Seq+1,
      timestamp=>os:system_time()
     },parsekey(HPrivKey)),
    BinTX=bin2hex:dbin2hex(NewTx),
    {
    tx:unpack(NewTx),
    httpc:request(post,
                  {"http://127.0.0.1:43280/api/tx/new",[],"application/json",<<"{\"tx\":\"0x",BinTX/binary,"\"}">>},
                  [],[{body_format,binary}])
    }.



    


