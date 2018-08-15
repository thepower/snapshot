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

-module(bsig).
-export([checksig/2, checksig1/2]).
-export([signhash/3, signhash1/3]).
-export([packsig/1, unpacksig/1]).
-export([pack_sign_ed/1, unpack_sign_ed/1]).
-export([add_sig/2]).

checksig1(BlockHash, SrcSig) ->
    HSig=unpacksig(SrcSig),
    #{binextra:=BExt, extra:=Xtra, signature:=Sig}=US=unpacksig(HSig),
    PubKey=proplists:get_value(pubkey, Xtra),
    Msg= <<BExt/binary, BlockHash/binary>>,
    case tpecdsa:secp256k1_ecdsa_verify(Msg, Sig, PubKey) of
        correct ->
            {true, US};
        _ ->
            false
    end.

checksig(BlockHash, Sigs) ->
    lists:foldl(
      fun(SrcSig, {Succ, Fail}) ->
              case checksig1(BlockHash, SrcSig) of
                  {true, US} ->
                      {[US|Succ], Fail};
                  false ->
                      {Succ, Fail+1}
              end
      end, {[], 0}, Sigs).

signhash1(MsgHash, ExtraData, PrivKey) ->
    BinExtra=pack_sign_ed(ExtraData),
    Msg= <<BinExtra/binary, MsgHash/binary>>,
    Signature=tpecdsa:secp256k1_ecdsa_sign(Msg, PrivKey, default, <<>>),
    <<255, (size(Signature)):8/integer, Signature/binary, BinExtra/binary>>.

signhash(MsgHash, ExtraData, PrivKey) ->
  PubKey=tpecdsa:secp256k1_ec_pubkey_create(PrivKey, true),
  signhash1(MsgHash, [{pubkey, PubKey}|ExtraData], PrivKey).

unpack_sign_ed(Bin) -> unpack_sign_ed(Bin, []).
unpack_sign_ed(<<>>, Acc) -> lists:reverse(Acc);
unpack_sign_ed(<<Attr:8/integer, Len:8/integer, Bin/binary>>, Acc) when Len<128 ->
    <<Val:Len/binary, Rest/binary>>=Bin,
    unpack_sign_ed(Rest, [decode_edval(Attr, Val)|Acc]).

pack_sign_ed(List) ->
    lists:foldl( fun({K, V}, Acc) ->
                         Val=encode_edval(K, V),
                         <<Acc/binary, Val/binary>>
                 end, <<>>, List).

% general pupose fields
decode_edval(1, <<Timestamp:64/big>>) -> {timestamp, Timestamp};
decode_edval(2, Bin) -> {pubkey, Bin};
decode_edval(3, <<TimeDiff:64/big>>) -> {createduration, TimeDiff};
decode_edval(4, <<TimeDiff:64/big>>) -> {createduration, TimeDiff};
decode_edval(240, <<KL:8/integer, Rest/binary>>=Raw) ->
  try
    <<Key:KL/binary, Val/binary>>=Rest,
    {Key, Val}
  catch _:_ ->
        {240, Raw}
  end;

decode_edval(254, Bin) -> {purpose, Bin};
decode_edval(255, Bin) -> {signature, Bin};
decode_edval(Key, BinVal) -> {Key, BinVal}.

encode_edval(timestamp, Integer) -> <<1, 8, Integer:64/big>>;
encode_edval(pubkey, PK) -> <<2, (size(PK)):8/integer, PK/binary>>;
encode_edval(createduration, Integer) -> <<3, 8, Integer:64/big>>;
encode_edval(signature, PK) -> <<255, (size(PK)):8/integer, PK/binary>>;
encode_edval(purpose, PK) -> <<254, (size(PK)):8/integer, PK/binary>>;
encode_edval(N, PK) when is_binary(N) andalso is_binary(PK) ->
  TS=size(N)+size(PK)+1,
  if TS>=64 ->
       throw('binkey_too_big');
     true ->
       <<240, TS:8/integer, (size(N)):8/integer, N/binary, PK/binary>>
  end;
encode_edval(_, _) -> <<>>.

splitsig(<<255, SLen:8/integer, Rest/binary>>) ->
    <<Signature:SLen/binary, Extradata/binary>>=Rest,
    {Signature, Extradata}.

unpacksig(HSig) when is_map(HSig) ->
    HSig;

unpacksig(BSig) when is_binary(BSig) ->
    {Signature, Hdr}=splitsig(BSig),
    #{ binextra => (Hdr),
       signature => (Signature),
       extra => unpack_sign_ed(Hdr)
     }.

packsig(BinSig) when is_binary(BinSig) ->
    BinSig;
packsig(#{signature:=Signature, binextra:=BinExtra}) ->
    <<255, (size(Signature)):8/integer, Signature/binary, BinExtra/binary>>.

add_sig(OldSigs, NewSigs) ->
  Apply=fun(#{extra:=EPL}=Sig, Acc) ->
           PK=proplists:get_value(pubkey, EPL),
           if PK == undefined -> Acc;
            is_binary(PK) ->
              case maps:is_key(PK, Acc) of
                true -> Acc;
                false -> maps:put(PK, Sig, Acc)
              end
           end
      end,
  Map1=lists:foldl(Apply, #{}, OldSigs),
  Map2=lists:foldl(Apply, Map1, NewSigs),
  maps:values(Map2).

