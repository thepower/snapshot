-module(scratchpad).
-compile(export_all).
-compile(nowarn_export_all).

node_id() ->
    {ok,K1}=application:get_env(tpnode,privkey),
    address:pub2addr(node,tpecdsa:secp256k1_ec_pubkey_create(hex:parse(K1), true)).

sign(Message) ->
    {ok,PKeyH}=application:get_env(tpnode,privkey),
    PKey=hex:parse(PKeyH),
    Msg32 = crypto:hash(sha256, Message),
    Sig = tpecdsa:secp256k1_ecdsa_sign(Msg32, PKey, default, <<>>),
    Pub=tpecdsa:secp256k1_ec_pubkey_create(PKey, true),
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
    {Found,tpecdsa:secp256k1_ecdsa_verify(Msg32, Sig, Public)}.


gentx(BFrom,BTo,Amount,HPrivKey) when is_binary(BFrom)->
    From=lists:filter(
           fun(A) when A>=$0 andalso $9>=A -> true;
              (A) when A>=$a andalso $z>=A -> true;
              (A) when A>=$A andalso $Z>=A -> true;
              (_) -> false
           end,binary_to_list(BFrom)),
    To=re:replace(binary_to_list(BTo),<<" ">>,"",[global,{return,binary}]),
    lager:info("From ~p to ~p",[From,To]),
    Cur= <<"FTT">>,
    inets:start(),
    C=try
          {ok,{{_HTTP11,200,_OK},_Headers,Body}}=
          httpc:request(get,{"http://127.0.0.1:43280/api/address/"++From,[]},[],[{body_format,binary}]),
          #{<<"info">>:=C1}=jsx:decode(Body,[return_maps]),
          C1
      catch _:_ ->
                #{
              <<"amount">> => #{},
              <<"seq">> => 0
             }
      end,
    #{<<"seq">>:=Seq}=maps:get(Cur,C,#{<<"amount">> => 0,<<"seq">> => 0}),

    Tx=#{
      amount=>Amount,
      cur=>Cur,
      extradata=>jsx:encode(#{
                   message=><<"preved from gentx">>
                  }),
      from=>list_to_binary(From),
      to=>To,
      seq=>Seq+1,
      timestamp=>os:system_time(millisecond)
     },
    io:format("TX1 ~p.~n",[Tx]),
    NewTx=tx:sign(Tx,address:parsekey(HPrivKey)),
    io:format("TX2 ~p.~n",[NewTx]),
    BinTX=bin2hex:dbin2hex(NewTx),
    io:format("TX3 ~p.~n",[BinTX]),
    {
    tx:unpack(NewTx),
    httpc:request(post,
                  {"http://127.0.0.1:43280/api/tx/new",[],"application/json",<<"{\"tx\":\"0x",BinTX/binary,"\"}">>},
                  [],[{body_format,binary}])
    }.

reapply_settings() ->
    {ok,HexPrivKey}=application:get_env(tpnode,privkey),
    PrivKey=hex:parse(HexPrivKey),
    Patch=settings:sign(
            settings:get_patches(gen_server:call(blockchain,settings)),
            PrivKey),
    io:format("PK ~p~n",[settings:verify(Patch)]),
    {Patch,
     gen_server:call(txpool, {patch, Patch})}.


test_alloc_block() ->
    {ok,HexPrivKey}=application:get_env(tpnode,privkey),
    PrivKey=hex:parse(HexPrivKey),
    Patch=settings:sign(
            settings:dmp(
              settings:mp(
                [
                 #{t=>set,p=>[current,allocblock,group], v=>10},
                 #{t=>set,p=>[current,allocblock,block], v=>2},
                 #{t=>set,p=>[current,allocblock,last], v=>0}
                ])),
      PrivKey),
    io:format("PK ~p~n",[settings:verify(Patch)]),
    {Patch
    %gen_server:call(txpool, {patch, Patch})
    }.


test_sign_patch() ->
    {ok,HexPrivKey}=application:get_env(tpnode,privkey),
    PrivKey=hex:parse(HexPrivKey),
    Patch=settings:sign(
            settings:dmp(
              settings:mp(
                [
                 %       #{t=>set,p=>[globals,patchsigs], v=>2},
                 #{t=>set,p=>[chain,0,blocktime], v=>10},
                 #{t=>set,p=>[chain,0,allowempty], v=>0}
                 %       #{t=>list_add,p=>[chains], v=>1},
                 %       #{t=>set,p=>[chain,1,minsig], v=>1},
                 %       #{t=>set,p=>[chain,1,blocktime], v=>60}
                 %       #{t=>set,p=>[nodechain,<<"node4">>], v=>1 },
                 %       #{t=>set,p=>[keys,<<"node4">>], v=>
                 %         hex:parse("02CB6107D2B19A01B0ABD6D9FCFF93D71227D03357DF9D48636D4968693FA8B540") }
                ])),
      PrivKey),
    io:format("PK ~p~n",[settings:verify(Patch)]),
    {Patch,
    gen_server:call(txpool, {patch, Patch})}.

outbound_tx() ->
    Pvt=address:parsekey(<<"5Kh9DfFypQNSd1GbGYNuHXsuaRcKVfcAWkrEQDJUMEZfi7yrvzm">>),
    Pub=tpecdsa:secp256k1_ec_pubkey_create(Pvt, false),
    From=address:pub2caddr(0,Pub),

    Pvt2=address:parsekey(<<"5JNg2WK9RUjxDranifcaTHrk5nGDnb1Pp2yq9Xfz3Arm6g93uCA">>),
    Pub2=tpecdsa:secp256k1_ec_pubkey_create(Pvt2, true),
    To=address:pub2caddr(1,Pub2),

    Cur= <<"FTT">>,
    Tx=#{
      amount=>3.34,
      seq=>19,
      cur=>Cur,
      extradata=>jsx:encode(#{
                   message=><<"send pi ftt">>
                  }),
      from=>From,
      to=>To,
      timestamp=>os:system_time(millisecond)
     },
    NewTx=tx:sign(Tx,Pvt),
    %tx:unpack(NewTx).
    txpool:new_tx(NewTx).


getpvt(Id) ->
    {ok, Pvt}=file:read_file(<<"tmp/addr",(integer_to_binary(Id))/binary,".bin">>),
    Pvt.

getpub(Id) ->
    Pvt=getpvt(Id),
    Pub=tpecdsa:secp256k1_ec_pubkey_create(Pvt, false),
    Pub.

rnd_key() ->
    <<B1:8/integer,_:31/binary>>=XPriv=crypto:strong_rand_bytes(32),
    if(B1==0) -> rnd_key();
      true -> XPriv
    end.

crypto() ->
    %{_,XPriv}=crypto:generate_key(ecdh, crypto:ec_curve(secp256k1)),
    XPriv=rnd_key(),
    XPub=calc_pub(XPriv),
    Pub=tpecdsa:secp256k1_ec_pubkey_create(XPriv, true),
    if(Pub==XPub) -> ok;
      true ->
          io:format("~p~n",[
                            {bin2hex:dbin2hex(Pub),
                             bin2hex:dbin2hex(XPub)}
                           ]),
          false
    end.

calc_pub(Priv) ->
    case crypto:generate_key(ecdh, crypto:ec_curve(secp256k1), Priv) of 
        {XPub,Priv} ->
            tpecdsa:minify(XPub);
        {_XPub, XPriv} ->
            lager:error_unsafe("Priv mismatch ~n~s~n~s",
                               [
                                bin2hex:dbin2hex(Priv),
                                bin2hex:dbin2hex(XPriv)
                               ]),
            error
    end.


sign1(Message) ->
    {ok,PKeyH}=application:get_env(tpnode,privkey),
    PKey=hex:parse(PKeyH),
    Sig = tpecdsa:secp256k1_ecdsa_sign(Message, PKey, default, <<>>),
    Pub=tpecdsa:secp256k1_ec_pubkey_create(PKey, true),
    <<(size(Pub)):8/integer,(size(Sig)):8/integer,Pub/binary,Sig/binary,Message/binary>>.

sign2(Message) ->
    {ok,PKeyH}=application:get_env(tpnode,privkey),
    PKey=hex:parse(PKeyH),
    Sig = crypto:sign(ecdsa, sha256, Message, [PKey, crypto:ec_curve(secp256k1)]),
    {Pub,PKey}=crypto:generate_key(ecdh, crypto:ec_curve(secp256k1), PKey),
    <<(size(Pub)):8/integer,(size(Sig)):8/integer,Pub/binary,Sig/binary,Message/binary>>.

check(<<PubLen:8/integer,SigLen:8/integer,Rest/binary>>) ->
    <<Pub:PubLen/binary,Sig:SigLen/binary,Message/binary>>=Rest,
    {Pub,Sig,Message}.
 
verify1(<<PubLen:8/integer,SigLen:8/integer,Rest/binary>>) ->
    <<Public:PubLen/binary,Sig:SigLen/binary,Message/binary>>=Rest,
    {ok,TrustedKeys}=application:get_env(tpnode,trusted_keys),
    Found=lists:foldl(
      fun(_,true) -> true;
         (HexKey,false) ->
              Pub=hex:parse(HexKey),
              Pub == Public
      end,false, TrustedKeys),
    {Found,tpecdsa:secp256k1_ecdsa_verify(Message, Sig, Public)}.

verify2(<<PubLen:8/integer,SigLen:8/integer,Rest/binary>>) ->
    <<Public:PubLen/binary,Sig:SigLen/binary,Message/binary>>=Rest,
    {ok,TrustedKeys}=application:get_env(tpnode,trusted_keys),
    Found=lists:foldl(
      fun(_,true) -> true;
         (HexKey,false) ->
              Pub=hex:parse(HexKey),
              Pub == Public
      end,false, TrustedKeys),
    {Found,crypto:verify(ecdsa, sha256, Message, Sig, [Public, crypto:ec_curve(secp256k1)])}.

test_pack_unpack(BlkID) ->
  B1=gen_server:call(blockchain,{get_block,BlkID}),
  B2=block:unpack(block:pack(B1)),
  file:write_file("tmp/pack_unpack_orig.txt", io_lib:format("~p.~n", [B1])),
  file:write_file("tmp/pack_unpack_pack.txt", io_lib:format("~p.~n", [B2])),
  B1==B2.
