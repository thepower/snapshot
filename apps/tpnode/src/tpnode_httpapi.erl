-module(tpnode_httpapi).

-export([h/3,after_filter/1,prettify_block/1]).

after_filter(Req) ->
    Origin=cowboy_req:header(<<"origin">>,Req,<<"*">>),
    Req1=cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req),
    Req2=cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST, OPTIONS">>, Req1),
    Req3=cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req2),
    Req4=cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"86400">>, Req3),
    cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req4).

h(Method, [<<"api">>|Path], Req) ->
    lager:info("Path ~p",[Path]),
    h(Method,Path,Req);

h(<<"GET">>, [<<"node">>,<<"status">>], _Req) ->
    {Chain, Header1} = gen_server:call(blockchain, status),
    Header=maps:map(
             fun(balroot,V) -> base64:encode(V);
                (ledger_hash,V) -> base64:encode(V);
                (parent,V) -> base64:encode(V);
                (setroot,V) -> base64:encode(V);
                (txroot,V) -> base64:encode(V);
                (_,V) -> V
             end, Header1),
    Peers=lists:map(
            fun(#{addr:=_Addr, auth:=Auth, state:=Sta}) ->
                    #{auth=>Auth,
                      state=>Sta
                     }
            end, tpic:peers()),
    SynPeers=gen_server:call(synchronizer,peers),
    {200,
     #{ result => <<"ok">>,
        status => #{
          nodeid=>nodekey:node_id(),
          public_key=>base64:encode(nodekey:get_pub()),
          chain=>Chain,
          header=>Header,
          tpic_peers=>Peers,
          sync_peers=>SynPeers
         }
      }};

h(<<"GET">>, [<<"miner">>,TAddr], _Req) ->
    {200,
     #{ result => <<"ok">>,
        mined=>naddress:mine(binary_to_integer(TAddr))
      }
    };

h(<<"GET">>, [<<"address">>,TAddr], _Req) ->
    try
    Addr=case TAddr of 
             <<"0x",Hex/binary>> ->
                 hex:parse(Hex);
             _ ->
                 naddress:decode(TAddr)
         end,
    Ledger=ledger:get([Addr]),
    case maps:is_key(Addr,Ledger) of
        false ->
            {404,
             #{ result => <<"not_found">>,
                address=>Addr
              }
            };
        true ->
            Info=maps:get(Addr,Ledger),
            InfoL=case maps:is_key(lastblk,Info) of
                      false ->
                          #{};
                      true ->
                          LastBlk=maps:get(lastblk,Info),
                          #{preblk=>LastBlk}
                  end,
            InfoU=case maps:is_key(ublk,Info) of
                      false ->
                          InfoL;
                      true ->
                          UBlk=maps:get(ublk,Info),
                          InfoL#{lastblk=>UBlk}
                  end,
            Info1=maps:merge(maps:remove(ublk,Info),InfoU),
            Info2=maps:map(
                    fun
                        (lastblk,V) -> bin2hex:dbin2hex(V);
                        (ublk,V) -> bin2hex:dbin2hex(V);
                        (pubkey,V) -> bin2hex:dbin2hex(V);
                        (preblk,V) -> bin2hex:dbin2hex(V);
                        (_,V) -> V
                    end, Info1),
            {200,
             #{ result => <<"ok">>,
                txtaddress=>naddress:encode(Addr),
                address=>bin2hex:dbin2hex(Addr),
                info=>Info2
              }
            }
    end
    catch throw:{error,address_crc} ->
              {200,
               #{ result => <<"error">>,
                  error=> <<"bad address">>
                }
              }
    end;

h(<<"POST">>, [<<"test">>,<<"tx">>], Req) ->
    {ok, ReqBody, _NewReq} = cowboy_req:read_body(Req),
    {200,
     #{ result => <<"ok">>,
        address=>ReqBody
      }
    };

h(<<"GET">>, [<<"block">>,BlockId], _Req) ->
    BlockHash0=if(BlockId == <<"last">>) -> last;
                true ->
                    hex:parse(BlockId)
              end,
    case gen_server:call(blockchain,{get_block,BlockHash0}) of
        undefined ->
            {404,
             #{ result=><<"error">>,
                error=><<"not found">>
              }
            };
        GoodBlock ->
            Block=prettify_block(GoodBlock),
            {200,
             #{ result => <<"ok">>,
                block => Block
              }
            }
    end;


h(<<"GET">>, [<<"settings">>], _Req) ->
    Block=blockchain:get_settings(),
    {200,
     #{ result => <<"ok">>,
        settings => Block
      }
    };


h(<<"POST">>, [<<"benchmark">>,N], _Req) ->
    Addresses=lists:map(
        fun(_) ->
                address:pub2addr(0,crypto:strong_rand_bytes(16))
        end, lists:seq(1, binary_to_integer(N))),
    {ok,Config}=application:get_env(tpnode,tpfaucet),
    Tokens=proplists:get_value(tokens,Config),
    Coin= <<"FTT">>,
    #{key:=Key, addr:=Adr}=proplists:get_value(Coin,Tokens,undefined),
    #{seq:=Seq0}=gen_server:call(blockchain,{get_addr,Adr,Coin}),
    BinKey=address:parsekey(Key),

    {_,Res}=lists:foldl(fun(Address,{Seq,Acc}) ->
                                Tx=#{
                                  amount=>1,
                                  cur=>Coin,
                                  extradata=>jsx:encode(#{
                                               message=> <<"Preved, ", Address/binary>>
                                              }),
                                  from=>Adr,
                                  to=>Address,
                                  seq=>Seq,
                                  timestamp=>os:system_time(millisecond)
                                 },
                                NewTx=tx:sign(Tx,BinKey),
                                case txpool:new_tx(NewTx) of
                                    {ok, TxID} ->
                                        {Seq+1,
                                         [#{addr=>Address,tx=>TxID}|Acc]
                                        };
                                    {error, Error} ->
                                        lager:error("Can't make tx: ~p",[Error]),
                                        {Seq+1,Acc}
                                end
                        end,{Seq0+1,[]},Addresses),
        {200,
     #{ result => <<"ok">>,
        address=>Res
      }
    };

h(<<"GET">>, [<<"give">>,<<"me">>,<<"money">>,<<"to">>,Address], Req) ->
    {RemoteIP,_Port}=cowboy_req:peer(Req),
    {ok,Config}=application:get_env(tpnode,tpfaucet),
    Faucet=proplists:get_value(register,Config),
    Tokens=proplists:get_value(tokens,Config),
    Res=lists:foldl(fun({Coin,Amount},Acc) ->
                        case proplists:get_value(Coin,Tokens,undefined) of
                            undefined -> Acc;
                            #{key:=Key,
                              addr:=Adr} ->
                                lager:info("Faucet ~p",[Adr]),
                                AddrState=case gen_server:call(blockchain,{get_addr,Adr,Coin}) of
                                              not_found -> bal:new();
                                              Found -> Found
                                          end,
                                Tx=#{
                                  amount=>Amount*1000000000,
                                  cur=>Coin,
                                  extradata=>jsx:encode(#{
                                               message=> <<"Welcome, ", Address/binary>>,
                                               ipaddress => list_to_binary(inet:ntoa(RemoteIP))
                                              }),
                                  from=>Adr,
                                  to=>naddress:decode(Address),
                                  seq=>bal:get(seq,AddrState)+1,
                                  timestamp=>os:system_time(millisecond)
                                 },
                                lager:info("Sign tx ~p",[Tx]),
                                NewTx=tx:sign(Tx,address:parsekey(Key)),
                                case txpool:new_tx(NewTx) of
                                    {ok, TxID} ->
                                        [#{c=>Coin,s=>Amount,tx=>TxID}|Acc];
                                    {error, Error} ->
                                        lager:error("Can't make tx: ~p",[Error]),
                                        Acc
                                end
                        end
                end,[],Faucet),
    {200,
     #{ result => <<"ok">>,
        address=>Address,
        addressb=>bin2hex:dbin2hex(naddress:decode(Address)),
        info=>Res
      }
    };




h(<<"POST">>, [<<"test">>,<<"request_fund">>], Req) ->
    Body=apixiom:bodyjs(Req),
    lager:info("B ~p",[Body]),
    Address=maps:get(<<"address">>,Body),
    ReqAmount=maps:get(<<"amount">>,Body,10),
    {ok,Config}=application:get_env(tpnode,tpfaucet),
    Faucet=proplists:get_value(register,Config),
    Tokens=proplists:get_value(tokens,Config),
    Res=lists:foldl(fun({Coin,CAmount},Acc) ->
                        case proplists:get_value(Coin,Tokens,undefined) of
                            undefined -> Acc;
                            #{key:=Key,
                              addr:=Adr} ->
                                Amount=min(CAmount,ReqAmount),
                                #{seq:=Seq}=gen_server:call(blockchain,{get_addr,Adr,Coin}),
                                Tx=#{
                                  amount=>Amount*1000000000,
                                  cur=>Coin,
                                  extradata=>jsx:encode(#{
                                               message=> <<"Test fund">>
                                              }),
                                  from=>Adr,
                                  to=>Address,
                                  seq=>Seq+1,
                                  timestamp=>os:system_time(millisecond)
                                 },
                                lager:info("Sign tx ~p",[Tx]),
                                NewTx=tx:sign(Tx,address:parsekey(Key)),
                                case txpool:new_tx(NewTx) of
                                    {ok, TxID} ->
                                        [#{c=>Coin,s=>Amount,tx=>TxID}|Acc];
                                    {error, Error} ->
                                        lager:error("Can't make tx: ~p",[Error]),
                                        Acc
                                end
                        end
                end,[],Faucet),
    {200,
     #{ result => <<"ok">>,
        address=>Address,
        info=>Res
      }
    };




h(<<"POST">>, [<<"register">>], Req) ->
    {_RemoteIP,_Port}=cowboy_req:peer(Req),
    Body=apixiom:bodyjs(Req),
    PKey=case maps:get(<<"public_key">>,Body) of
              <<"0x",BArr/binary>> ->
                  hex:parse(BArr);
              Any -> 
                  base64:decode(Any)
          end,

    BinTx=tx:pack( #{ type=>register, register=>PKey }),
    %{TX0,
    %gen_server:call(txpool, {register, TX0})
    %}.

    case txpool:new_tx(BinTx) of
        {ok, Tx} -> 
            {200,
             #{ result => <<"ok">>,
                pkey=>bin2hex:dbin2hex(PKey),
                txid => Tx
              }
            };
        {error, Err} ->
            lager:info("error ~p",[Err]),
            {500,
             #{ result => <<"error">>,
                pkey=>bin2hex:dbin2hex(PKey),
                tx=>base64:encode(BinTx),
                error => iolist_to_binary(io_lib:format("bad_tx:~p",[Err]))
              }
            }
    end;

h(<<"POST">>, [<<"address">>], Req) ->
    [Body]=apixiom:bodyjs(Req),
    lager:debug("New tx from ~s: ~p",[Body]),
    A=hd(Body), 
    R=naddress:encode(A),
    {200,
     #{ result => <<"ok">>,
        r=> R
      }
    };



h(<<"POST">>, [<<"tx">>,<<"debug">>], Req) ->
    {RemoteIP,_Port}=cowboy_req:peer(Req),
    Body=apixiom:bodyjs(Req),
    lager:debug("New tx from ~s: ~p",[inet:ntoa(RemoteIP), Body]),
    BinTx=case maps:get(<<"tx">>,Body,undefined) of
              <<"0x",BArr/binary>> ->
                  hex:parse(BArr);
              Any -> 
                  base64:decode(Any)
          end,
    X=tx:unpack(BinTx),
    {200,
     #{ result => <<"ok">>,
        tx => iolist_to_binary(io_lib:format("~p",[X]))
      }
    };

h(<<"POST">>, [<<"tx">>,<<"new">>], Req) ->
    {RemoteIP,_Port}=cowboy_req:peer(Req),
    Body=apixiom:bodyjs(Req),
    lager:debug("New tx from ~s: ~p",[inet:ntoa(RemoteIP), Body]),
    BinTx=case maps:get(<<"tx">>,Body,undefined) of
              <<"0x",BArr/binary>> ->
                  hex:parse(BArr);
              Any -> 
                  base64:decode(Any)
          end,
    %lager:info_unsafe("New tx ~p",[BinTx]),
    case txpool:new_tx(BinTx) of
        {ok, Tx} -> 
            {200,
             #{ result => <<"ok">>,
                txid => Tx
              }
            };
        {error, Err} ->
            lager:info("error ~p",[Err]),
            {500,
             #{ result => <<"error">>,
                error => iolist_to_binary(io_lib:format("bad_tx:~p",[Err]))
              }
            }
    end;

h(<<"OPTIONS">>, _, _Req) ->
    {200, [], ""};

h(_Method, [<<"status">>], Req) ->
    {RemoteIP,_Port}=cowboy_req:peer(Req),
    lager:info("Join from ~p",[inet:ntoa(RemoteIP)]),
    %Body=apixiom:bodyjs(Req),

    {200,
     #{ result => <<"ok">>,
        client => list_to_binary(inet:ntoa(RemoteIP))
      }
    }.

%PRIVATE API

prettify_block(#{}=Block0) ->
    maps:map(
      fun(sign,Signs) ->
              show_signs(Signs);
         (hash,BlockHash) ->
              bin2hex:dbin2hex(BlockHash);
         (child,BlockHash) ->
              bin2hex:dbin2hex(BlockHash);
         (bals,Bal) ->
              maps:fold(
                fun(BalAddr,V,A) ->
                       FixedBal=case maps:is_key(lastblk,V) of
                           false ->
                               maps:remove(ublk,V);
                           true ->
                               LastBlk=maps:get(lastblk,V),
                                 maps:put(lastblk,
                                          bin2hex:dbin2hex(LastBlk),
                                          maps:remove(ublk,V)
                                         )
                       end,
                       PrettyBal=maps:map(
                                 fun(pubkey,PubKey) ->
                                         bin2hex:dbin2hex(PubKey);
                                     (_BalKey,BalVal) -> 
                                         BalVal
                                 end, FixedBal),
                       maps:put(bin2hex:dbin2hex(BalAddr),PrettyBal,A)
                end, #{}, Bal);
         (header,BlockHeader) ->
              maps:map(
                fun(parent,V) ->
                        bin2hex:dbin2hex(V);
                   (_K,V) when is_binary(V) andalso size(V) == 32 ->
                        bin2hex:dbin2hex(V);
                   (_K,V) ->
                        V
                end, BlockHeader);
         (settings,Settings) ->
              lists:map(
                fun({CHdr,CBody}) ->
                        %DMP=settings:dmp(CBody),
                        %DMP=base64:encode(CBody),
                        {CHdr, maps:map(
                                 fun(patch,Payload) ->
                                         settings:dmp(Payload);
                                    (signatures,Sigs) ->
                                         show_signs(Sigs);
                                    (_K,V) -> V
                                 end, CBody)}
                end, 
                Settings
               );
         (inbound_blocks,IBlocks) ->
              lists:map(
                fun({BHdr,BBody}) ->
                        {BHdr, 
                         prettify_block(BBody)
                        }
                end, 
                IBlocks
               );

         (tx_proof,Proof) ->
              lists:map(
                fun({CHdr,CBody}) ->
                        {CHdr, 
                         [bin2hex:dbin2hex(H) || H<-tuple_to_list(CBody)]
                        }
                end, 
                Proof
               );
         (txs, TXS) ->
              lists:map(
                fun({TxID,TXB}) ->
                        {TxID,
                         maps:map(
                           fun(register, Val) ->
                                   bin2hex:dbin2hex(Val);
                              (from, <<Val:8/binary>>) ->
                                   bin2hex:dbin2hex(Val);
                              (to, <<Val:8/binary>>) ->
                                   bin2hex:dbin2hex(Val);
                              (address, Val) ->
                                   bin2hex:dbin2hex(Val);
                              (sig,#{}=V1) -> 
                                 [
                                  {bin2hex:dbin2hex(SPub),
                                   bin2hex:dbin2hex(SPri)} || {SPub,SPri} <- maps:to_list(V1) ];
                           (_,V1) -> V1
                           end, maps:without([public_key,signature],TXB))
                        }
                end, 
                TXS
               );
         (_,V) ->
              V
      end, Block0);

prettify_block(#{hash:=<<0,0,0,0,0,0,0,0>>}=Block0) -> 
    Block0#{ hash=><<"0000000000000000">> }.

show_signs(Signs) ->
    lists:map(
      fun(BSig) ->
              #{binextra:=Hdr,
                extra:=Extra,
                signature:=Signature}=bsig:unpacksig(BSig),
              #{ binextra => bin2hex:dbin2hex(Hdr),
                 signature => bin2hex:dbin2hex(Signature),
                 extra =>
                 lists:map(
                   fun({K,V}) ->
                           if(is_binary(V)) ->
                                 {K,bin2hex:dbin2hex(V)};
                             true ->
                                 {K,V}
                           end
                   end, Extra
                  )
               }
      end, Signs).


