-module(block).
-export([blkid/1]).
-export([mkblock/1,binarizetx/1,extract/1,outward_mk/2,outward_mk/1]).
-export([verify/1,outward_verify/1,sign/2,sign/3,sigverify/2]).
-export([prepack/1]).
-export([pack/1,unpack/1]).
-export([packsig/1,unpacksig/1]).
-export([split_packet/2, split_packet/1, glue_packet/1]).

-export([bals2bin/1]).

-ifndef(TEST).
-define(TEST,1).
-endif.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

unpack_mproof(M) ->
	list_to_tuple(
	lists:map(
	  fun(E) when is_binary(E) -> E;
		 (E) when is_list(E) -> unpack_mproof(E)
	  end, M)).

pack_mproof(M) ->
	lists:map(
	  fun(E) when is_binary(E) -> E;
		 (E) when is_tuple(E) -> pack_mproof(E);
		 (E) when is_list(E) -> pack_mproof(E)
	  end, 
	  if is_tuple(M) ->
			 tuple_to_list(M);
		 is_list(M) ->
			 M
	  end).

prepack(Block) ->
	maps:map(
	  fun(bals,BalsSnap) ->
			  maps:fold(
				fun(Address,Snap,Acc) ->
						maps:put(Address,bal:pack(Snap),Acc)
				end,#{},BalsSnap);
		 (sync,SyncState) ->
			  maps:fold(
				fun(Chain,{BlkNo,BlkHash},Acc) ->
						maps:put(Chain,[BlkNo,BlkHash],Acc)
				end,#{},SyncState);
		 (sign,Sigs) ->
			  lists:map(
				fun(Sig) ->
						bsig:packsig(Sig)
				end, Sigs);
		 (settings,Txs) ->
			  maps:from_list(
				lists:map(
				  fun({TxID,T}) ->
						  {TxID,tx:pack(T)}
				  end, Txs)
			   );
		 (inbound_blocks,Blocks) ->
			  maps:from_list(
				lists:map(
				  fun({TxID,T}) ->
						  {TxID,block:pack(T)}
				  end, Blocks)
			   );
		 (outbound,Txp) ->
			  lager:notice("FIXME: save outbound flag in tx"),
			  lists:map(
				fun({TxID,Cid}) ->
						[TxID,Cid]
				end, Txp
			   );
		 (tx_proof,Txp) ->
			  lists:map(
				fun({TxID,MProof}) ->
						[TxID,pack_mproof(MProof)];
				   (Any) ->
						throw({dont_know_how_to_pack,Any})
				end, Txp
			   );
		 (extdata,ED) ->
			  lists:map(
				fun({Key,Val}) ->
						[Key,Val]
				end, ED
			   );
		 (txs,Txs) ->
			  lists:map(
				fun({TxID,T}) ->
						[TxID,tx:pack(T)]
				end, Txs
			   );
		 (_,V) ->
			  V
	  end,
	  Block
	 ).
pack(Block) ->
    Prepare=prepack(Block),
%    file:write_file("tmp/origblk.txt",[io_lib:format("~p.~n",[Block])]),
%    file:write_file("tmp/prepblk.txt",[io_lib:format("~p.~n",[Prepare])]),
    Packed=msgpack:pack(Prepare),
    if is_binary(Packed) ->
           Packed;
       true ->
		   file:write_file("log/pack_error.txt",
						   [io_lib:format("~p.~n",[Block])]),
           throw({cant_pack,Packed})
    end.

unpack(Block) when is_binary(Block) ->
	Atoms=[hash,outbound,header,settings,txs,sign,bals,
		   balroot,ledger_hash,height,parent,txroot,tx_proof,
		   amount,lastblk,seq,t,child,setroot,
		   inbound_blocks,chain,extdata],
    case msgpack:unpack(Block,[{known_atoms, Atoms}]) of
        {ok, Hash} ->
            maps:map(
              fun
                  (bals,BalsSnap) ->
                      maps:fold(
                        fun(Address,BinSnap,Acc) ->
                                maps:put(Address,bal:unpack(BinSnap),Acc)
                        end,#{},BalsSnap);
                  (sync,SyncState) ->
                      maps:fold(
                        fun(Chain,[BlkNo,BlkHash],Acc) ->
                                maps:put(Chain,{BlkNo,BlkHash},Acc)
                        end,#{},SyncState);
                  (outbound,TXs) ->
                      lists:map(
                        fun([TxID,Cid]) ->
                                {TxID, Cid}
                        end, TXs);
                  (tx_proof,TXs) ->
                      lists:map(
                        fun([TxID,Proof]) ->
                                {TxID, unpack_mproof(Proof)}
                        end, TXs);
                  (txs,TXs) ->
                      lists:map(
                        fun([TxID,Tx]) ->
                                {TxID, tx:unpack(Tx)}
                        end, TXs);
                  (extdata,ED) ->
                      lists:map(
                        fun([Key,Val]) ->
                                {Key, Val}
                        end, ED);
                  (inbound_blocks,Blocks) ->
                      lists:map(
                        fun({TxID,T}) ->
                                {TxID,block:unpack(T)}
                        end, maps:to_list(Blocks)
                       );
                  (settings,Txs) ->
                      lists:map(
                        fun({TxID,T}) ->
                                {TxID,tx:unpack(T)}
                        end, maps:to_list(Txs)
                       );
                  (sign,Sigs) ->
                      lists:map(
                        fun(Sig) ->
                                bsig:unpacksig(Sig)
                        end, Sigs);
                  (_,V) ->
                      V
              end, Hash);
        {error,Err} ->
            throw({block_unpack,Err})
    end.



outward_verify(#{ header:=#{parent:=Parent, height:=H}=Header, 
                  hash:=HdrHash, 
                  sign:=Sigs
                }=Blk) ->
    try
        Txs=maps:get(txs,Blk,[]),
        Txp=maps:get(tx_proof,Blk,[]),
        BTxs=binarizetx(Txs),

        TxRoot=maps:get(txroot,Header,undefined),
        BalsRoot=maps:get(balroot,Header,undefined),
        SettingsRoot=maps:get(setroot,Header,undefined),

        BHeader=lists:foldl(
                  fun({_,undefined},ABHhr) ->
                          ABHhr;
                     ({_N,Root},ABHhr) ->
                          <<ABHhr/binary,
                            Root/binary
                          >>
                  end,
                  <<H:64/integer, %8
                    Parent/binary
                  >>,
                  [{txroot,TxRoot},
                   {balroot,BalsRoot},
                   {ledger_hash,maps:get(ledger_hash, Header, undefined)},
                   {setroot,SettingsRoot}
                  ]
                 ),
        Hash=crypto:hash(sha256,BHeader),
        if Hash =/= HdrHash -> throw(false); true -> ok end,
        TxFail=lists:foldl(
                 fun(_,true) -> 
                         true;
                    ({TxID,TxBin},false) ->
                         Proof=proplists:get_value(TxID,Txp),
                         Res=gb_merkle_trees:verify_merkle_proof(TxID,
                                                                 TxBin,
                                                                 TxRoot,
                                                                 Proof),
                         Res=/=ok
                 end, false, BTxs),
        if TxFail -> throw(false); true -> ok end,

        {true, bsig:checksig(Hash, Sigs)}

    catch throw:false ->
              false
    end.

sigverify(#{hash:=Hash}, Sigs) ->
	bsig:checksig(Hash, Sigs).

verify(#{ header:=#{parent:=Parent, 
                    height:=H
                   }=Header, 
          hash:=HdrHash, 
          sign:=Sigs
        }=Blk) ->

    HLedgerHash=maps:get(ledger_hash,Header,undefined),
    Txs=maps:get(txs,Blk,[]),
    Bals=maps:get(bals,Blk,#{}),
    Settings=maps:get(settings,Blk,[]),

    BTxs=binarizetx(Txs),
    TxMT=gb_merkle_trees:from_list(BTxs),
    BalsBin=bals2bin(Bals),
    BalsMT=gb_merkle_trees:from_list(BalsBin),
    BSettings=binarize_settings(Settings),
    SettingsMT=gb_merkle_trees:from_list(BSettings),

    TxRoot=gb_merkle_trees:root_hash(TxMT),
    BalsRoot=gb_merkle_trees:root_hash(BalsMT),
    SetRoot=gb_merkle_trees:root_hash(SettingsMT),
	HeaderItems=[{txroot,TxRoot},
				 {balroot,BalsRoot},
				 {ledger_hash,HLedgerHash},
				 {setroot,SetRoot}|
				 case maps:is_key(chain,Header) of
					 false -> [];
					 true ->
						 [{chain, maps:get(chain,Header)}]
				 end],

    {BHeader,_Hdr}=build_header(HeaderItems,Parent,H),

    Hash=crypto:hash(sha256,BHeader),
    %io:format("H1 ~s ~nH2 ~s~n~n",[bin2hex:dbin2hex(Hash),
    %                             bin2hex:dbin2hex(HdrHash)]),
    if Hash =/= HdrHash ->
           HSetRoot=maps:get(setroot,Header,undefined),
           HTxRoot=maps:get(txroot,Header,undefined),
           HBalsRoot=maps:get(balroot,Header,undefined),

           if TxRoot =/= HTxRoot ->
                  lager:notice("TX root mismatch ~s vs ~s",
                              [
                                bin2hex:dbin2hex(TxRoot),
                                bin2hex:dbin2hex(HTxRoot)
                              ]);
              SetRoot =/= HSetRoot ->
                  lager:notice("Set root mismatch",
                               [
                                bin2hex:dbin2hex(SetRoot),
                                bin2hex:dbin2hex(HSetRoot)
                               ]);
              BalsRoot =/= HBalsRoot ->
                  lager:notice("Bals root mismatch ~s vs ~s",
                               [
                                bin2hex:dbin2hex(BalsRoot),
                                bin2hex:dbin2hex(HBalsRoot)
                               ]);
              true ->
                  lager:notice("Something mismatch")
           end,
           false;
       true ->
           {true, bsig:checksig(Hash, Sigs)}
    end.


binarize_settings([]) -> [];
binarize_settings([{TxID,#{ patch:=_LPatch }=Patch}|Rest]) ->
    [{TxID,tx:pack(Patch)}|binarize_settings(Rest)].


mkblock(#{ txs:=Txs, parent:=Parent, height:=H, bals:=Bals, settings:=Settings }=Req) ->
    LH=maps:get(ledger_hash,Req,undefined),
    Txsl=lists:keysort(1,lists:usort(Txs)),
    BTxs=binarizetx(Txsl),
    TxMT=gb_merkle_trees:from_list(BTxs),
    %TxHash=crypto:hash(sha256,BTxs),
    BalsBin=bals2bin(Bals),
    BalsMT=gb_merkle_trees:from_list(BalsBin),
    BSettings=binarize_settings(Settings),
    SettingsMT=gb_merkle_trees:from_list(BSettings),

    TxRoot=gb_merkle_trees:root_hash(TxMT),
    BalsRoot=gb_merkle_trees:root_hash(BalsMT),
    SettingsRoot=gb_merkle_trees:root_hash(SettingsMT),

	HeaderItems=[{txroot,TxRoot},
				 {balroot,BalsRoot},
				 {ledger_hash,LH},
				 {setroot,SettingsRoot}|
				 case maps:is_key(mychain,Req) of
					 false -> [];
					 true ->
						 [{chain, maps:get(mychain,Req)}]
				 end],
    {BHeader,Hdr}=build_header(HeaderItems,Parent,H),
	%lager:info("HI ~p",[Hdr]),

    Block=#{header=>Hdr,
      hash=>crypto:hash(sha256,BHeader),
      txs=>Txsl,
      bals=>Bals,
      settings=>Settings,
      sign=>[] },
    Block1=case maps:get(tx_proof, Req, []) of
               [] ->
                   Block;
               List ->
                   Proof=lists:map(
                           fun(TxID) ->
                                   {TxID,gb_merkle_trees:merkle_proof (TxID,TxMT)}
                           end, List),
                   maps:put(tx_proof,Proof,Block)
           end,
	Block2=case maps:get(inbound_blocks, Req, []) of
			   [] ->
				   Block1;
			   List2 ->
				   maps:put(inbound_blocks,
							lists:map(
							  fun({InBlId, InBlk}) ->
									  {InBlId, maps:remove(txs,InBlk)} 
							  end,List2),
							Block1)
		   end,
	case maps:get(extdata, Req, []) of
		[] ->
			Block2;
		List3 ->
			maps:put(extdata,List3,Block2)
	end;

mkblock(Blk) ->
    case maps:is_key(settings,Blk) of
        false -> 
            mkblock(maps:put(settings, [], Blk));
        true ->
            io:format("s ~p~n",[maps:keys(Blk)]),
            throw(badmatch)
    end.

outward_mk(Block) -> 
    outward(maps:get(outbound,Block,[]),Block,#{}).

outward_mk(TxS,Block) -> 
    outward(TxS,Block,#{}).

outward([],Block,Acc) -> 
    maps:map(
      fun(_K,{Tx,Tp}) ->
              MiniBlock=maps:with([hash,header,sign],Block),
              MiniBlock#{ txs=>Tx, tx_proof=>Tp }
      end,
      Acc);

outward([{TxID,Chain}|Rest],#{txs:=Txs,tx_proof:=Proofs}=Block,Acc) ->
    {ChainTx,ChainTp}=maps:get(Chain,Acc,{[],[]}),
    Tx=proplists:get_value(TxID,Txs),
    Proof=proplists:get_value(TxID,Proofs),
    outward(Rest,
              Block,
              maps:put(Chain,
                       { [{TxID,Tx}|ChainTx], [{TxID,Proof}|ChainTp] } ,Acc)
             ).


binarizetx([]) ->
    [];

binarizetx([{TxID,Tx}|Rest]) ->
    BTx=tx:pack(Tx),
    %TxIDLen=size(TxID),
    %TxLen=size(BTx),
    %<<TxIDLen:8/integer,TxLen:16/integer,TxID/binary,BTx/binary,(binarizetx(Rest))/binary>>.
    [{TxID,BTx}|binarizetx(Rest)].

extract(<<>>) ->
    [];

extract(<<TxIDLen:8/integer,TxLen:16/integer,Body/binary>>) ->
    <<TxID:TxIDLen/binary,Tx:TxLen/binary,Rest/binary>> = Body,
    [{TxID,tx:unpack(Tx)}|extract(Rest)].

    
bals2bin(NewBal) ->
    L=lists:keysort(1,maps:to_list(NewBal)),
    lists:foldl(
      fun({Addr, %generic bal
           #{amount:=_}=Bal
          } ,Acc) ->
              %TODO: check with integer addresses
              [{Addr, bal:pack(Bal)}|Acc];
         ({Addr, %port
           #{chain:=NewChain}
          },Acc) ->
              [{<<Addr/binary>>,
                <<"pout", NewChain:64/big>>}|Acc];
		 (Any,_) -> 
			  throw({"Bad bal",Any})
      end, [], L).

blkid(<<X:8/binary,_/binary>>) ->
    bin2hex:dbin2hex(X).


sign(Blk, ED, PrivKey) when is_map(Blk) ->
    Hash=maps:get(hash,Blk),
	%There is needs for tests, but breakes packing.
	%TODO: Fix
    %Sign=bsig:unpacksig(bsig:signhash(Hash, ED, PrivKey)),
    Sign=bsig:signhash(Hash, ED, PrivKey),
    Blk#{
      sign=>[Sign|maps:get(sign,Blk,[])]
     }.

sign(Blk, PrivKey) when is_map(Blk) ->
    Timestamp=os:system_time(millisecond),
    ED=[{timestamp,Timestamp}],
    sign(Blk, ED, PrivKey).

build_header(HeaderItems,Parent,H) ->
	lists:foldl(
	  fun({_,undefined},{ABHhr,AHdr}) ->
			  {ABHhr,AHdr};
		 ({chain,ChainNo},{ABHhr,AHdr}) ->
			  {
			   <<ABHhr/binary, ChainNo:64/big >>,
			   maps:put(chain,ChainNo,AHdr)
			  };
		 ({N,Root},{ABHhr,AHdr}) ->
			  {
			   <<ABHhr/binary,
				 Root/binary
			   >>,
			   maps:put(N,Root,AHdr)
			  }
	  end,
	  {
	   <<H:64/integer, %8
		 Parent/binary
	   >>,
	   #{ parent=>Parent, height=>H }
	  }, 
	  HeaderItems	
	 ).

unpacksig(Block) ->
	maps:map(
	  fun(sign,Sigs) ->
			  lists:map(
				fun(Sig) ->
						bsig:unpacksig(Sig)
				end, Sigs);
		 (_,Val) -> Val
	  end, Block).

packsig(Block) ->
	maps:map(
	  fun(sign,Sigs) ->
			  lists:map(
				fun(Sig) ->
						bsig:packsig(Sig)
				end, Sigs);
		 (_,Val) -> Val
	  end, Block).

split_packet(Data) ->
    Size = 32,
    Length = ceil(byte_size(Data) / Size),
    split_packet(Size, Data, 1, Length).
split_packet(Size, Data) ->
    Length = ceil(byte_size(Data) / Size),
    split_packet(Size, Data, 1, Length).
split_packet(Size, Data, Seq, Length) when Size > 0 ->
    case Data of
        <<Packet:Size/binary, Rest/binary>> ->
            [<<Seq:32, Length:32, Packet/binary>> | split_packet(Size, Rest, Seq + 1, Length)];
        <<>> ->
            [];
        _ ->
            [<<Seq:32, Length:32, Data/binary>>]
    end.

glue_packet(List) ->
    SortedList = lists:sort(fun(<<N1:32, _/binary>>, <<N2:32, _/binary>>) -> N1 =< N2 end, List),
    {_, _, Valid} = lists:foldl(fun(<<Seq:32, Length:32, _/binary>>, {PrevSeq, PrevLength, Acc}) ->
                                   {Seq, Length, Acc and (PrevSeq + 1 =:= Seq) and (PrevLength =:= Length)}
                                end, {0, length(SortedList), true}, SortedList),
    if Valid ->
           list_to_binary(lists:map(fun(<<_:32, _:32, Val/binary>>) -> Val end, SortedList));
       true ->
           lager:error("Received block is broken ~p", [SortedList])
    end.

-ifdef(TEST).
block_test_() ->
    Priv1Key= <<200,100,200,100,200,100,200,100,200,100,200,100,200,100,200,100,
                200,100,200,100,200,100,200,100,200,100,200,100,200,100,200,100>>,
    Priv2Key= <<200,300,200,100,200,100,200,100,200,100,200,100,200,100,200,100,
                200,300,200,100,200,100,200,100,200,100,200,100,200,100,200,100>>,
    Pub1=tpecdsa:secp256k1_ec_pubkey_create(Priv1Key, true),
    Pub2=tpecdsa:secp256k1_ec_pubkey_create(Priv2Key, true),
    RPatch=[
             #{t=>set,p=>[globals,patchsigs], v=>2},
             #{t=>set,p=>[chains], v=>[0]},
             #{t=>set,p=>[chain,0,minsig], v=>2},
             #{t=>set,p=>[keys,node1], v=>Pub1},
             #{t=>set,p=>[keys,node2], v=>Pub2},
             #{t=>set,p=>[nodechain], v=>#{node1=>0, node2=>0 } }
            ],
    SPatch1= settings:sign(RPatch, Priv1Key),
    SPatch2= settings:sign(SPatch1, Priv2Key),
    lager:info("SPatch ~p",[SPatch2]),
    NewB=mkblock(#{ parent=><<0,0,0,0,0,0,0,0>>,
                    height=>0,
                    txs=>[],
                    bals=>#{},
					mychain=>100500,
                    settings=>[
                               {
                                <<"patch123">>,
                                SPatch2
                               }
                              ],
                    sign=>[]
                  }
          ),
    Block=sign(sign(NewB,Priv1Key),Priv2Key),
    Repacked=unpack(pack(Block)),
    [
    ?_assertMatch({true,{_,0}},verify(Block)),
    ?_assertEqual(packsig(Block),packsig(Repacked)),
    ?_assertEqual(unpacksig(Block),Repacked),
    ?_assertEqual(unpacksig(Block),unpacksig(Repacked)),
    ?_assertEqual(Block,packsig(Repacked)),
    ?_assertEqual(true,lists:all(
                         fun(#{extra:=E}) ->
                                 P=proplists:get_value(pubkey,E),
                                 P==Pub2 orelse P==Pub1
                         end,
                         element(1,element(2,verify(Block))))
                         ),
    ?_assertEqual(split_packet(2, <<1, 2, 3, 4, 5>>), [<<0,0,0,1,0,0,0,3,1,2>>, <<0,0,0,2,0,0,0,3,3,4>>, <<0,0,0,3,0,0,0,3,5>>]),
    ?_assertEqual(split_packet(2, <<1, 2, 3, 4>>), [<<0,0,0,1,0,0,0,3,1,2>>, <<0,0,0,2,0,0,0,3,3,4>>]),
    ?_assertEqual(split_packet(3, <<1, 2, 3, 4>>), [<<0,0,0,1,0,0,0,2,1,2,3>>, <<0,0,0,2,0,0,0,2,4>>]),
    ?_assertEqual(split_packet(2, <<>>), []),
    ?_assertError({badmatch,{}}, glue_packet([])),
    ?_assertEqual(glue_packet([<<0,0,0,1,0,0,0,2,1,2>>, <<0,0,0,2,0,0,0,2,3,4>>]), <<1, 2, 3, 4>>),
    ?_assertEqual(glue_packet([<<0,0,0,1,0,0,0,3,1,2>>, <<0,0,0,2,0,0,0,3,3,4>>, <<0,0,0,2,0,0,0,3,5>>]), <<1, 2, 3, 4, 5>>)
    ].

-endif.

