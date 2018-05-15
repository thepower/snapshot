-module(tpnode_httpapi).

-export([h/3, after_filter/1, prettify_block/2, prettify_block/1]).

after_filter(Req) ->
  Origin=cowboy_req:header(<<"origin">>, Req, <<"*">>),
  Req1=cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
                                  Origin, Req),
  Req2=cowboy_req:set_resp_header(<<"access-control-allow-methods">>,
                                  <<"GET, POST, OPTIONS">>, Req1),
%  Req3=cowboy_req:set_resp_header(<<"access-control-allow-credentials">>,
%                                  <<"true">>, Req2),
  Req4=cowboy_req:set_resp_header(<<"access-control-max-age">>,
                                  <<"86400">>, Req2),
  Req5=cowboy_req:set_resp_header(<<"tpnode-name">>, nodekey:node_name(), Req4),
  Req6=cowboy_req:set_resp_header(<<"tpnode-id">>, nodekey:node_id(), Req5),
  cowboy_req:set_resp_header(<<"access-control-allow-headers">>,
                             <<"content-type">>, Req6).

h(Method, [<<"api">>|Path], Req) ->
  lager:info("Path ~p", [Path]),
  h(Method, Path, Req);

h(<<"GET">>, [<<"node">>, <<"status">>], _Req) ->
  {Chain, Hash, Header1} = case catch gen_server:call(blockchain, status) of
                             {A, B, C} -> {A, B, C};
                             _Err ->
                               lager:info("Error ~p", [_Err]),
                               {-1, <<0, 0, 0, 0, 0, 0, 0, 0>>, #{}}
                           end,
  QS=cowboy_req:parse_qs(_Req),
  BinPacker=case proplists:get_value(<<"bin">>, QS) of
              <<"b64">> -> fun(Bin) -> base64:encode(Bin) end;
              <<"hex">> -> fun(Bin) -> hex:encode(Bin) end;
              <<"raw">> -> fun(Bin) -> Bin end;
              _ -> fun(Bin) -> base64:encode(Bin) end
            end,
  Header=maps:map(
           fun(_, V) when is_binary(V) -> BinPacker(V);
              (_, V) -> V
           end, Header1),
  Peers=lists:map(
          fun(#{addr:=_Addr, auth:=Auth, state:=Sta, authdata:=AD}) ->
              #{auth=>Auth,
                state=>Sta,
                node=>proplists:get_value(nodeid, AD, null)
               };
             (#{addr:=_Addr}) ->
              #{auth=>unknown,
                state=>unknown
               }
          end, tpic:peers()),
  SynPeers=gen_server:call(synchronizer, peers),
  {Ver, _BuildTime}=tpnode:ver(),
  {200,
   #{ result => <<"ok">>,
      status => #{
        nodeid=>nodekey:node_id(),
        public_key=>BinPacker(nodekey:get_pub()),
        blockchain=>#{
          chain=>Chain,
          hash=>BinPacker(Hash),
          header=>Header
         },
        xchain_inbound => try
                            gen_server:call(xchain_dispatcher, peers)
                          catch _:_ -> #{ error => true }
                          end,
        xchain_outbound => try
                             gen_server:call(xchain_client, peers)
                           catch _:_ -> #{ error => true }
                           end,
        tpic_peers=>Peers,
        sync_peers=>SynPeers,
        ver=>list_to_binary(Ver)
       }
    }};

h(<<"GET">>, [<<"miner">>, TAddr], _Req) ->
  {200,
   #{ result => <<"ok">>,
      mined=>naddress:mine(binary_to_integer(TAddr))
    }
  };

h(<<"GET">>, [<<"contract">>, TAddr, <<"call">>, Method | Args], _Req) ->
  try
    Addr=case TAddr of
           <<"0x", Hex/binary>> ->
             hex:parse(Hex);
           _ ->
             naddress:decode(TAddr)
         end,
    Ledger=ledger:get([Addr]),
    case maps:is_key(Addr, Ledger) of
      false ->
        {404,
         #{ result => <<"not_found">>,
            address=>Addr
          }
        };
      true ->
        Info=maps:get(Addr, Ledger),
        VMName=maps:get(vm, Info),
        {ok,List}=smartcontract:get(VMName,Method,Args,Info),

        {200,
         #{ address=><<"0x",(hex:encode(Addr))/binary>>,
            result=>List
          }
        }
    end
  catch throw:{error, address_crc} ->
          {200,
           #{ result => <<"error">>,
              error=> <<"invalid address">>
            }
          }
  end;

h(<<"GET">>, [<<"contract">>, TAddr], _Req) ->
  try
    Addr=case TAddr of
           <<"0x", Hex/binary>> ->
             hex:parse(Hex);
           _ ->
             naddress:decode(TAddr)
         end,
    Ledger=ledger:get([Addr]),
    case maps:is_key(Addr, Ledger) of
      false ->
        {404,
         #{ result => <<"not_found">>,
            address=>Addr
          }
        };
      true ->
        Info=maps:get(Addr, Ledger),
        VMName=maps:get(vm, Info),
        {ok,CN,CD}=smartcontract:info(VMName),
        {ok,List}=smartcontract:getters(VMName),

        {200,
         #{ result => <<"ok">>,
            txtaddress=>naddress:encode(Addr),
            address=><<"0x",(hex:encode(Addr))/binary>>,
            contract=>CN,
            descr=>CD,
            getters=>List
          }
        }
    end
  catch throw:{error, address_crc} ->
          {200,
           #{ result => <<"error">>,
              error=> <<"invalid address">>
            }
          }
  end;

h(<<"GET">>, [<<"where">>, TAddr], _Req) ->
  try
    Addr=case TAddr of
           <<"0x", Hex/binary>> ->
             hex:parse(Hex);
           _ ->
             naddress:decode(TAddr)
         end,
    #{block:=Blk}=naddress:parse(Addr),
    MyChain=blockchain:chain(),
    if(MyChain==Blk) ->
        case ledger:get(Addr) of
          not_found ->
            {404, 
             #{result=><<"not_found">>,
               address=><<"0x",(hex:encode(Addr))/binary>>,
               txtaddress=>naddress:encode(Addr)
              }
            };
          #{} ->
            {200,
             #{ result => <<"found">>,
                chain=>Blk,
                address=><<"0x",(hex:encode(Addr))/binary>>,
                txtaddress=>naddress:encode(Addr)
              }
            }
        end;
      true ->
        {200,
         #{ result => <<"other_chain">>,
            chain=>Blk,
            address=><<"0x",(hex:encode(Addr))/binary>>,
            txtaddress=>naddress:encode(Addr)
          }
        }
    end
  catch throw:{error, address_crc} ->
          {400,
           #{ result => <<"error">>,
              error=> <<"invalid address">>
            }
          };
          throw:bad_addr ->
          {400,
           #{ result => <<"error">>,
              error=> <<"invalid address">>
            }
          }
  end;


h(<<"GET">>, [<<"address">>, TAddr], _Req) ->
  try
    Addr=case TAddr of
           <<"0x", Hex/binary>> ->
             hex:parse(Hex);
           _ ->
             naddress:decode(TAddr)
         end,
    Ledger=ledger:get([Addr]),
    case maps:is_key(Addr, Ledger) of
      false ->
        {404,
         #{ result => <<"not_found">>,
            address=>Addr
          }
        };
      true ->
        Info=maps:get(Addr, Ledger),
        InfoL=case maps:is_key(lastblk, Info) of
                false ->
                  #{};
                true ->
                  LastBlk=maps:get(lastblk, Info),
                  #{preblk=>LastBlk}
              end,
        InfoU=case maps:is_key(ublk, Info) of
                false ->
                  InfoL;
                true ->
                  UBlk=maps:get(ublk, Info),
                  InfoL#{lastblk=>UBlk}
              end,
        Info1=maps:merge(maps:remove(ublk, Info), InfoU),
        Info2=maps:map(
                fun
                  (lastblk, V) -> bin2hex:dbin2hex(V);
      (ublk, V) -> bin2hex:dbin2hex(V);
      (pubkey, V) -> bin2hex:dbin2hex(V);
      (preblk, V) -> bin2hex:dbin2hex(V);
      (code, V) -> base64:encode(V);
      (state, V) ->
        try
          iolist_to_binary(
            io_lib:format("~p",
                          [
                           erlang:binary_to_term(V, [safe])])
           )
        catch _:_ ->
                base64:encode(V)
        end;
      (_, V) -> V
                end, Info1),
        Info3=try
                Contract=maps:get(vm, Info2),
                lager:error("C1 ~p",[Contract]),
                CV=smartcontract:info(Contract),
                lager:error("C2 ~p",[CV]),
                {ok, VN, VD} = CV,
                maps:put(contract, [VN,VD], Info2)
              catch _:_ ->
                      lager:error("NC"),
                      Info2
              end,
        {200,
         #{ result => <<"ok">>,
            txtaddress=>naddress:encode(Addr),
            address=><<"0x",(hex:encode(Addr))/binary>>,
            info=>Info3
          }
        }
    end
  catch throw:{error, address_crc} ->
          {400,
           #{ result => <<"error">>,
              error=> <<"invalid address">>
            }
          };
          throw:bad_addr ->
          {400,
           #{ result => <<"error">>,
              error=> <<"invalid address">>
            }
          }
  end;

h(<<"POST">>, [<<"test">>, <<"tx">>], Req) ->
  {ok, ReqBody, _NewReq} = cowboy_req:read_body(Req),
  {200,
   #{ result => <<"ok">>,
      address=>ReqBody
    }
  };

h(<<"GET">>, [<<"blockinfo">>, BlockId], _Req) ->
  QS=cowboy_req:parse_qs(_Req),
  BinPacker=case proplists:get_value(<<"bin">>, QS) of
              <<"b64">> -> fun(Bin) -> base64:encode(Bin) end;
              <<"hex">> -> fun(Bin) -> bin2hex:dbin2hex(Bin) end;
              <<"raw">> -> fun(Bin) -> Bin end;
              _ -> fun(Bin) -> bin2hex:dbin2hex(Bin) end
            end,
  BlockHash0=if(BlockId == <<"last">>) -> last;
               true ->
                 hex:parse(BlockId)
             end,
  case gen_server:call(blockchain, {get_block, BlockHash0}) of
    undefined ->
      {404,
       #{ result=><<"error">>,
          error=><<"not found">>
        }
      };
    #{txs:=Txl}=GoodBlock ->
      ReadyBlock=maps:put(
                   txs_count,
                   length(Txl),
                   maps:without([txs,bals],GoodBlock)
                  ),
      Block=prettify_block(ReadyBlock, BinPacker),
      {200,
       #{ result => <<"ok">>,
          block => Block
        }
      }
  end;


h(<<"GET">>, [<<"block">>, BlockId], _Req) ->
  QS=cowboy_req:parse_qs(_Req),
  BinPacker=case proplists:get_value(<<"bin">>, QS) of
              <<"b64">> -> fun(Bin) -> base64:encode(Bin) end;
              <<"hex">> -> fun(Bin) -> bin2hex:dbin2hex(Bin) end;
              <<"raw">> -> fun(Bin) -> Bin end;
              _ -> fun(Bin) -> bin2hex:dbin2hex(Bin) end
            end,
  Address=case proplists:get_value(<<"addr">>, QS) of
            undefined -> undefined;
            Addr -> naddress:decode(Addr)
          end,

  BlockHash0=if(BlockId == <<"last">>) -> last;
               true ->
                 hex:parse(BlockId)
             end,
  case gen_server:call(blockchain, {get_block, BlockHash0}) of
    undefined ->
      {404,
       #{ result=><<"error">>,
          error=><<"not found">>
        }
      };
    GoodBlock ->
      ReadyBlock=if Address == undefined ->
                      GoodBlock;
                    is_binary(Address) ->
                      filter_block(
                        GoodBlock,
                        Address)
                 end,
      Block=prettify_block(ReadyBlock, BinPacker),
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

h(<<"POST">>, [<<"register">>], Req) ->
  {_RemoteIP, _Port}=cowboy_req:peer(Req),
  Body=apixiom:bodyjs(Req),
  PKey=case maps:get(<<"public_key">>, Body) of
         <<"0x", BArr/binary>> ->
           hex:parse(BArr);
         Any ->
           base64:decode(Any)
       end,

  BinTx=tx:pack( #{ type=>register,
                    register=>PKey,
                    pow=>maps:get(<<"pow">>,Body,<<>>),
                    timestamp=>maps:get(<<"timestamp">>,Body,0)
                  }),
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
      lager:info("error ~p", [Err]),
      {500,
       #{ result => <<"error">>,
          pkey=>bin2hex:dbin2hex(PKey),
          tx=>base64:encode(BinTx),
          error => iolist_to_binary(io_lib:format("bad_tx:~p", [Err]))
        }
      }
  end;

h(<<"POST">>, [<<"address">>], Req) ->
  [Body]=apixiom:bodyjs(Req),
  lager:debug("New tx from ~s: ~p", [Body]),
  A=hd(Body),
  R=naddress:encode(A),
  {200,
   #{ result => <<"ok">>,
      r=> R
    }
  };

h(<<"GET">>, [<<"emulation">>, <<"start">>], _Req) ->
  R = case txgen:start_link() of
        {ok, _} -> #{ok => true, res=> <<"Started">>};
        {error, {already_started, _}} ->
          case txgen:is_running() of
            true -> #{ok => false, res=> <<"Already running">>};
            false->
              txgen:restart(),
              #{ok => true, res=> <<"Started">>}
          end;
        _ -> #{ok => false, res=> <<"Error">>}
      end,
  {200, #{res => R}};

h(<<"GET">>, [<<"tx">>, <<"status">>, TxID], _Req) ->
  R=txstatus:get_json(TxID),
  {200, #{res=>R}};

h(<<"POST">>, [<<"tx">>, <<"debug">>], Req) ->
  {RemoteIP, _Port}=cowboy_req:peer(Req),
  Body=apixiom:bodyjs(Req),
  lager:info("New DEBUG from ~s: ~p", [inet:ntoa(RemoteIP), Body]),
  BinTx=case maps:get(<<"tx">>, Body, undefined) of
          <<"0x", BArr/binary>> ->
            hex:parse(BArr);
          Any ->
            base64:decode(Any)
        end,
  Dbg=case maps:get(<<"debug">>, Body, undefined) of
        <<"0x", BArr1/binary>> ->
          hex:parse(BArr1);
        Any1 ->
          base64:decode(Any1)
      end,
  U=tx:unpack(BinTx),
  lager:info("Debug TX ~p",[U]),
  Dbg2=tx:mkmsg(U),
  lager:info("Debug1 ~p",[bin2hex:dbin2hex(Dbg)]),
  lager:info("Debug2 ~p",[bin2hex:dbin2hex(Dbg2)]),
  XBin=io_lib:format("~p",[U]),
  XTx=case tx:verify1(U) of
        {ok, Tx} ->
          io_lib:format("~p.~n",[Tx]);
        Err ->
          io_lib:format("~p.~n",[{error,Err}])
      end,

  lager:info("Res ~p",[#{
               xtx=>iolist_to_binary(XTx),
               dbg=>iolist_to_binary(XBin)
              }]),
  {200,
   #{
                                       xtx=>iolist_to_binary(XTx),
                                       dbg=>iolist_to_binary(XBin)
                                      }
  };

h(<<"POST">>, [<<"tx">>, <<"new">>], Req) ->
  {RemoteIP, _Port}=cowboy_req:peer(Req),
  Body=apixiom:bodyjs(Req),
  lager:debug("New tx from ~s: ~p", [inet:ntoa(RemoteIP), Body]),
  BinTx=case maps:get(<<"tx">>, Body, undefined) of
          <<"0x", BArr/binary>> ->
            hex:parse(BArr);
          Any ->
            base64:decode(Any)
        end,
  %lager:info_unsafe("New tx ~p", [BinTx]),
  case txpool:new_tx(BinTx) of
    {ok, Tx} ->
      {200,
       #{ result => <<"ok">>,
          txid => Tx
        }
      };
    {error, Err} ->
      lager:info("error ~p", [Err]),
      {500,
       #{ result => <<"error">>,
          error => iolist_to_binary(io_lib:format("bad_tx:~p", [Err]))
        }
      }
  end;

h(<<"OPTIONS">>, _, _Req) ->
  {200, [], ""};

h(_Method, [<<"status">>], Req) ->
  {RemoteIP, _Port}=cowboy_req:peer(Req),
  lager:info("Join from ~p", [inet:ntoa(RemoteIP)]),
  %Body=apixiom:bodyjs(Req),

  {200,
   #{ result => <<"ok">>,
      client => list_to_binary(inet:ntoa(RemoteIP))
    }
  }.

%PRIVATE API

filter_block(Block, Address) ->
  maps:map(
    fun(bals, B) ->
        maps:with([Address], B);
       (txs, B) ->
        [ {TxID, TX} || {TxID, #{from:=F, to:=T}=TX} <- B, F==Address orelse T==Address ];
       (_, V) -> V
    end, Block).

prettify_block(Block) ->
  prettify_block(Block, fun(Bin) -> bin2hex:dbin2hex(Bin) end).

prettify_block(#{}=Block0, BinPacker) ->
  maps:map(
    fun(sign, Signs) ->
        show_signs(Signs, BinPacker);
       (hash, BlockHash) ->
        BinPacker(BlockHash);
       (child, BlockHash) ->
        BinPacker(BlockHash);
       (bals, Bal) ->
        maps:fold(
          fun(BalAddr, V, A) ->
              FixedBal=case maps:is_key(lastblk, V) of
                         false ->
                           maps:remove(ublk, V);
                         true ->
                           LastBlk=maps:get(lastblk, V),
                           maps:put(lastblk,
                                    BinPacker(LastBlk),
                                    maps:remove(ublk, V)
                                   )
                       end,
              PrettyBal=maps:map(
                          fun(pubkey, PubKey) ->
                              BinPacker(PubKey);
                             (_BalKey, BalVal) ->
                              BalVal
                          end, FixedBal),
              maps:put(BinPacker(BalAddr), PrettyBal, A)
          end, #{}, Bal);
       (header, BlockHeader) ->
        maps:map(
          fun(parent, V) ->
              BinPacker(V);
             (_K, V) when is_binary(V) andalso size(V) == 32 ->
              BinPacker(V);
             (_K, V) ->
              V
          end, BlockHeader);
       (settings, Settings) ->
        lists:map(
          fun({CHdr, CBody}) ->
              %DMP=settings:dmp(CBody),
              %DMP=base64:encode(CBody),
              {CHdr, maps:map(
                       fun(patch, Payload) ->
                           settings:dmp(Payload);
                          (signatures, Sigs) ->
                           show_signs(Sigs, BinPacker);
                          (_K, V) -> V
                       end, CBody)}
          end,
          Settings
         );
       (inbound_blocks, IBlocks) ->
        lists:map(
          fun({BHdr, BBody}) ->
              {BHdr,
               prettify_block(BBody, BinPacker)
              }
          end,
          IBlocks
         );

       (tx_proof, Proof) ->
        lists:map(
          fun({CHdr, CBody}) ->
              {CHdr,
               [BinPacker(H) || H<-tuple_to_list(CBody)]
              }
          end,
          Proof
         );
       (txs, TXS) ->
        lists:map(
          fun({TxID, TXB}) ->
              {TxID,
               maps:map(
                 fun(register, Val) ->
                     BinPacker(Val);
                    (from, <<Val:8/binary>>) ->
                     BinPacker(Val);
                    (to, <<Val:8/binary>>) ->
                     BinPacker(Val);
                    (address, Val) ->
                     BinPacker(Val);
                    (invite, Val) ->
                     BinPacker(Val);
                    (pow, Val) ->
                     BinPacker(Val);
                    (sig, #{}=V1) ->
                     [
                      {BinPacker(SPub),
                       BinPacker(SPri)} || {SPub, SPri} <- maps:to_list(V1) ];
                    (_, V1) -> V1
                 end, maps:without([public_key, signature], TXB))
              }
          end,
          TXS
         );
       (_, V) ->
        V
    end, Block0);

prettify_block(#{hash:=<<0, 0, 0, 0, 0, 0, 0, 0>>}=Block0, BinPacker) ->
  Block0#{ hash=>BinPacker(<<0:64/big>>) }.

show_signs(Signs, BinPacker) ->
  lists:map(
    fun(BSig) ->
        #{binextra:=Hdr,
          extra:=Extra,
          signature:=Signature}=bsig:unpacksig(BSig),
        UExtra=lists:map(
                 fun({K, V}) ->
                     if(is_binary(V)) ->
                         {K, BinPacker(V)};
                       true ->
                         {K, V}
                     end
                 end, Extra
                ),
        NodeID=proplists:get_value(pubkey, Extra, <<>>),
        #{ binextra => BinPacker(Hdr),
           signature => BinPacker(Signature),
           extra =>UExtra,
           '_nodeid' => nodekey:node_id(NodeID),
           '_nodename' => try chainsettings:is_our_node(NodeID)
                          catch _:_ -> null
                          end
         }
    end, Signs).


