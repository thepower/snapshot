%%% -------------------------------------------------------------------
%%% "ThePower.io". Copyright (C) 2018 Mihaylenko Maxim, Belousov Igor
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

-module(tpecdsa).
-export([generate_priv/0, minify/1, calc_pub/2, sign/2, verify/3]).
-export([secp256k1_ecdsa_sign/4,
  secp256k1_ecdsa_verify/3,
  secp256k1_ec_pubkey_create/2,
  secp256k1_ec_pubkey_create/1,
  export/2,
  import/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("public_key/include/public_key.hrl").

generate_priv() ->
  generate_priv(10).

generate_priv(0) ->
  throw('cant_generate');
generate_priv(N) ->
  case crypto:generate_key(ecdh, crypto:ec_curve(secp256k1)) of
    {_, <<255, 255, 255, 255, 255, 255, 255, 255, 255, 255, _/binary>>} ->
      %avoid priv keys with leading ones
      generate_priv(N-1);
    {_, <<Private:32/binary>>} ->
      Private;
    _ ->
      %avoid priv keys with leading zeros
      generate_priv(N-1)
  end.

minify(XPub) ->
  case XPub of
    <<Gxp:8/integer, Gy:32/binary, _:31/binary>> when Gxp==2 orelse Gxp==3->
      <<Gxp:8/integer, Gy:32/binary>>;
    <<4, Gy:32/binary, _:31/binary, Gxx:8/integer>> ->
      Gxp=if Gxx rem 2 == 1 -> 3;
            true -> 2
          end,
      <<Gxp:8/integer, Gy:32/binary>>
  end.

calc_pub(Priv, false) -> %full X
  {XPub, _XPrivi} = crypto:generate_key(ecdh, crypto:ec_curve(secp256k1), Priv),
  %it's ok. regenerated priv can be less tnan 32 bytes, is it with leading zeros
  %BigInt=binary:decode_unsigned(XPrivi),
  %Priv= <<BigInt:256/integer>>,
  XPub;

calc_pub(Priv, true) -> %compact X
  {XPub, _XPrivi} = crypto:generate_key(ecdh, crypto:ec_curve(secp256k1), Priv),
  %it's ok. regenerated priv can be less tnan 32 bytes, is it with leading zeros
  %BigInt=binary:decode_unsigned(XPrivi),
  %Priv= <<BigInt:256/integer>>,
  tpecdsa:minify(XPub).

sign(Message, PrivKey) ->
  crypto:sign(ecdsa, sha256, Message, [PrivKey, crypto:ec_curve(secp256k1)]).

verify(Message, Public, Sig) ->
  R=crypto:verify(ecdsa, sha256, Message, Sig, [Public, crypto:ec_curve(secp256k1)]),
  if R -> correct;
    true -> incorrect
  end.


secp256k1_ecdsa_sign(Msg32, SecKey, _Nonce, _NonceData) ->
  sign(Msg32, SecKey).

secp256k1_ecdsa_verify(Msg32, Sig, Pubkey) ->
  verify(Msg32, Pubkey, Sig).

secp256k1_ec_pubkey_create(SecKey) -> %use mini keys by default
  calc_pub(SecKey, true).

secp256k1_ec_pubkey_create(SecKey, Compressed) ->
  calc_pub(SecKey, Compressed).


export(<<PrivKey:32/binary>>,pem) ->
  BDer=base64:encode(export(PrivKey,der)),
  <<"-----BEGIN EC PRIVATE KEY-----\n",
    BDer/binary,
  "\n-----END EC PRIVATE KEY-----">>;

export(<<PrivKey:32/binary>>,der) ->
  <<16#30,16#2e,16#02,16#01,16#01,16#04,16#20,PrivKey/binary,
    16#a0,16#07,16#06,16#05,16#2b,16#81,16#04,16#00,16#0a>>;

export(<<PrivKey:32/binary>>,raw) ->
  PrivKey;

export(<<PubKey:33/binary>>,pem) ->
  BDer=base64:encode(export(PubKey,der)),
  <<"-----BEGIN PUBLIC KEY-----\n",
    BDer/binary,
  "\n-----END PUBLIC KEY-----">>;

export(<<PubKey:33/binary>>,der) ->
  <<48,54,48,16,6,7,42,134,72,206,61,2,1,6,5,43,129,4,0,10,3,34,0,
    PubKey/binary>>;

export(<<PubKey:33/binary>>,raw) ->
  PubKey;

export(<<PubKey:65/binary>>,pem) ->
  BDer=base64:encode(export(PubKey,der)),
  <<"-----BEGIN PUBLIC KEY-----\n",
    BDer/binary,
  "\n-----END PUBLIC KEY-----">>;

export(<<PubKey:65/binary>>,der) ->
  <<48,(54+32),48,16,6,7,42,134,72,206,61,2,1,6,5,43,129,4,0,10,3,(34+32),0,
    PubKey/binary>>;

export(<<PubKey:65/binary>>,raw) ->
  PubKey.

import(<<"---",_/binary>>=PEM) ->
  [{KeyType, DerKey, not_encrypted}] = public_key:pem_decode(PEM),
  case public_key:der_decode(KeyType, DerKey) of
    #'ECPrivateKey'{
      version = 1,
      privateKey = PrivKey,
      parameters = {namedCurve,{1,3,132,0,10}}
     } ->
      {priv, PrivKey};
    #'SubjectPublicKeyInfo'{
%       algorithm = #'AlgorithmIdentifier'{ algorithm={1,2,840,10045,2,1}},
       subjectPublicKey = PubKey
      } ->
      {pub, PubKey}
  end.

-ifdef(TEST).
sign_test() ->
  SecKey = <<128, 128, 128, 128, 128, 128, 128, 128,
						 128, 128, 128, 128, 128, 128, 128, 128,
						 128, 128, 128, 128, 128, 128, 128, 128,
						 128, 128, 128, 128, 128, 128, 128, 128>>,
  Msg32 = <<"Hello">>,
  << _/binary >> = Sig = secp256k1_ecdsa_sign(Msg32, SecKey, default, <<>>),
  ?assertEqual(correct,
							 secp256k1_ecdsa_verify(Msg32, Sig,
																			secp256k1_ec_pubkey_create(SecKey, false)
																		 )
							),
  ?assertEqual(correct,
							 secp256k1_ecdsa_verify(Msg32, Sig,
																			secp256k1_ec_pubkey_create(SecKey, true)
																		 )
							),
  ?assertEqual(incorrect,
							 secp256k1_ecdsa_verify(<<1, Msg32/binary>>,
																			Sig,
																			secp256k1_ec_pubkey_create(SecKey, false)
																		 )
							).
-endif.
