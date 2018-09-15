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

-module(address).

-export([pub2caddr/2, pub2addr/2, pub2addrraw/2, check/1,
         encodekey/1, parsekey/1, paddr/1,
        addr2chain/2]).

-define(VER, 133).

paddr(PKey) ->
    Crc=erlang:crc32(PKey),
    <<"tp", (base58:encode( << Crc:32/integer, PKey/binary>> ))/binary>>.

pub2addr(node, Pub) ->
    Hash=crypto:hash(ripemd160, Pub),
    Crc=erlang:crc32(Hash),
    base58:encode( <<76, 200, 56, 214, Crc:32/integer, Hash/binary>> );

pub2addr(Ver, Pub) when is_integer(Ver) ->
    H2H3=pub2addrraw(Ver, Pub),
    base58:encode(H2H3);

pub2addr({ver, Ver}, Pub) when is_integer(Ver) ->
    H2H3=pub2addrraw(Ver, Pub),
    base58:encode(H2H3);

pub2addr({chain, Chain}, Pub) when is_integer(Chain) ->
    H2H3=pub2caddrraw(Chain, Pub),
    base58:encode(H2H3).

pub2caddr(Chain, Pub) ->
    H2H3=pub2caddrraw(Chain, Pub),
    base58:encode(H2H3).

pub2addrraw(Ver, Pub) ->
    H1=crypto:hash(ripemd160,
                   crypto:hash(sha256, Pub)
                  ),
    H2= <<Ver:8/integer, H1/binary>>,
    <<H3:4/binary, _/binary>>=crypto:hash(sha256, crypto:hash(sha256, H2)),
    <<H2/binary, H3/binary>>.

pub2caddrraw(Chain, Pub) ->
    H1=crypto:hash(ripemd160,
                   crypto:hash(sha256, Pub)
                  ),
    H2= <<?VER, H1/binary, Chain:32/big>>,
    <<H3:4/binary, _/binary>>=crypto:hash(sha256, H2),
    <<H2/binary, H3/binary>>.

addr2chain(Chain, Address) ->
    H2H3=addr2chainraw(Chain, Address),
    base58:encode(H2H3).

addr2chainraw(Chain, Address) ->
    case base58:decode(Address) of
        <<Ver:8/integer, RipeMD:20/binary, Check:4/binary>> ->
            <<H3:4/binary, _/binary>>=
            crypto:hash(sha256,
                        crypto:hash(sha256, <<Ver:8/integer, RipeMD:20/binary>>)
                       ),
            Check=H3,
            H2= <<?VER, RipeMD/binary, Chain:32/big>>,
            <<H3n:4/binary, _/binary>>=crypto:hash(sha256, H2),
            <<H2/binary, H3n/binary>>;
        <<?VER, RipeMD:20/binary, OldChain:32/big, Check:4/binary>> ->
            <<H3:4/binary, _/binary>>=
                        crypto:hash(sha256, <<?VER, RipeMD:20/binary, OldChain:32/big>>),
            Check=H3,
            H2= <<?VER, RipeMD/binary, Chain:32/big>>,
            <<H3n:4/binary, _/binary>>=crypto:hash(sha256, H2),
            <<H2/binary, H3n/binary>>
    end.

check(Address) ->
    try
        case base58:decode(Address) of
            <<Ver:8/integer, RipeMD:20/binary, Check:4/binary>> ->
                <<H3:4/binary, _/binary>>=
                crypto:hash(sha256,
                            crypto:hash(sha256, <<Ver:8/integer, RipeMD:20/binary>>)
                           ),
                {Check==H3, {ver, Ver}};
            <<?VER, RipeMD:20/binary, OldChain:32/big, Check:4/binary>> ->
                <<H3:4/binary, _/binary>>=
                crypto:hash(sha256, <<?VER, RipeMD:20/binary, OldChain:32/big>>),
                {Check==H3, {chain, OldChain}};
            _ ->
                {false, unknown}
        end
    catch _:_ ->
              {false, invalid}
    end.

encodekey(Pvt) ->
    H2= <<128, Pvt/binary, 1>>,
    <<H3:4/binary, _/binary>>=crypto:hash(sha256, crypto:hash(sha256, H2)),
    base58:encode(<<H2/binary, H3/binary>>).

parsekey(<<"0x", BKey/binary>>) ->
    hex:parse(BKey);
parsekey(Base58) ->
    B58Decode=base58:decode(Base58),
    KS=size(B58Decode)-5,
    case B58Decode of
        <<128, KeyBody:KS/binary, KC:4/binary>> ->
            <<H3:4/binary, _/binary>>=
            crypto:hash(sha256,
                        crypto:hash(sha256, <<128:8/integer, KeyBody/binary>>)
                       ),
            if(KC==H3 andalso KS==32) ->
                  KeyBody;
              (KC==H3 andalso KS==33) ->
                  <<KB:32/binary, _:1/binary>>=KeyBody,
                  KB;
              true ->
                  error
            end;
        _ ->
            error
    end.


