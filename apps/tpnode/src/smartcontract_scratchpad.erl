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

-module(smartcontract_scratchpad).
-compile(export_all).
-compile(nowarn_export_all).

hello(Lang, Ver) when is_integer(Ver), is_list(Lang) ->
  msgpack:pack(#{
    null => "hello",
    ver => Ver,
    lang => Lang
   }).
%> smartcontract_scratchpad:hello("test",1).
%83C4046C616E67A474657374C0A568656C6C6FC40376657201


run(Address, Ledger, Tx, GasLimit) ->
  msgpack:pack(
    #{
    address => Address,
    ledger => Ledger,
    tx => Tx,
    gas => GasLimit}
              ).

% > io:format("~s~n",[hex:encode(smartcontract_scratchpad:run(<<128,1,64,0,4,0,0,2>>,ledger:get(<<128,1,64,0,4,0,0,2>>),smartcontract_scratchpad:test_tx(),10000))]).
% 84C40761646472657373C4088001400004000002C403676173CD2710C4066C656467657287C406616D6F756E7480C404636F6465C413837400000001640008696E74657276616C610AC4076C617374626C6BC4200A9DE6C7412C3ED1E890FE169E0214317D456A31DEDED749D08C063C35C6E836C4067075626B6579C421036A21F068BE92697268B60D96C4CA93052EC104E49E003AE2C404F916864372F4C4057374617465C421837400000002640008696E74657276616C610A6400066C6173745F6862FFFFFFFBC40475626C6BC42038975C877A8C784EBD1A3E317EC0F5EA0AD5DDD1AE0F21F15ADCEC9EC94966A2C402766DC408636861696E666565C4027478C4B883C40373696781C421036A21F068BE92697268B60D96C4CA93052EC104E49E003AE2C404F916864372F4C4463044021F5E103CD1A94398A9F25EE53EEC161F768061018B6EC07CE8D8E520EE28224A02210090E3066A3F190F5C0DD3C8356CE68270B5F92D331F7C1D5007E3D957B8005312C402747897C4088001400004000001C40880014000040000020AA3465454CF00000164376E9E930BB47B226D657373616765223A22707265766564227DC40474797065A27478
% #{<<"address">> => <<128,1,64,0,4,0,0,2>>,
%      <<"gas">> => 10000,
%      <<"ledger">> =>
%          #{<<"amount">> => #{},
%            <<"code">> =>
%                <<131,116,0,0,0,1,100,0,8,105,110,116,101,114,118,97,108,
%                  97,10>>,
%            <<"lastblk">> =>
%                <<10,157,230,199,65,44,62,209,232,144,254,22,158,2,20,49, 125,69,106,49,222,...>>,
%            <<"pubkey">> =>
%                <<3,106,33,240,104,190,146,105,114,104,182,13,150,196,202, 147,5,46,193,4,...>>,
%            <<"state">> =>
%                <<131,116,0,0,0,2,100,0,8,105,110,116,101,114,118,97,108, 97,10,...>>,
%            <<"ublk">> =>
%                <<56,151,92,135,122,140,120,78,189,26,62,49,126,192,245, 234,10,213,...>>,
%            <<"vm">> => <<"chainfee">>},
%      <<"tx">> =>
%          <<131,196,3,115,105,103,129,196,33,3,106,33,240,104,190, 146,105,114,104,182,13,150,196,...>>}




test_tx() ->
    TestPriv=address:parsekey(<<"5KHwT1rGjWiNzoZeFuDT85tZ6KTTZThd4xPfaKWRUKNqvGQQtqK">>),
    From= <<128, 1, 64, 0, 4, 0, 0, 1>>,
    To= <<128, 1, 64, 0, 4, 0, 0, 2>>,
    Cur= <<"FTT">>,
    Seq=10,
    Tx=#{
      amount=>10,
      cur=>Cur,
      extradata=>jsx:encode(#{ message=><<"preved">> }),
      from=>From,
      to=>To,
      seq=>Seq+1,
      timestamp=>os:system_time(millisecond)
     },
    NewTx=tx:sign(Tx, TestPriv),
    NewTx.
% > io:format("~s~n",[hex:encode(smartcontract_scratchpad:test_tx())]).
% 83C40373696781C421036A21F068BE92697268B60D96C4CA93052EC104E49E003AE2C404F916864372F4C448304602210089EB6AB605258EAE7D1A5E5D3E65C2C19E735DAF3CF0C5C7DA03838A32413A1F022100BA28E17CD3944B6C143B716A2D2AADF9828AD26ADBDBDB87620517F563DA1173C402747897C4088001400004000001C40880014000040000020AA3465454CF00000164376F7E340BB47B226D657373616765223A22707265766564227DC40474797065A27478
% #{amount => 10,cur => <<"FTT">>,
%  extradata => <<"{\"message\":\"preved\"}">>,
%  from => <<128,1,64,0,4,0,0,1>>,
%  seq => 11,
%  sig =>
%      #{<<3,106,33,240,104,190,146,105,114,104,182,13,150,196,
%          202,147,5,46,193,4,228,158,...>> =>
%            <<48,68,2,32,89,0,151,88,204,171,69,255,57,67,79,14,65,
%              226,101,26,65,122,...>>},
%  timestamp => 1529938447916,
%  to => <<128,1,64,0,4,0,0,2>>,
%  type => tx}
   

%deploy tx
%83C40373696781C421036A21F068BE92697268B60D96C4CA93052EC104E49E003AE2C404F916864372F4C4473045022065CE08F4EE92DB55E4F48CC96A976636635DCD4B95FC03053D51FFACBCABDAC30221008C8479E09FAA619EEA36EDA1FAD50C4E9D5AAB9006ADC1132B59147C446675E7C402747895C4088001400004000002CF00000164378E67E001C408636861696E666565C413837400000001640008696E74657276616C610AC40474797065A66465706C6F79
%
%#{<<"sig">> =>
%          #{<<3,106,33,240,104,190,146,105,114,104,182,13,150,196,
%              202,147,5,46,193,4,228,158,0,58,226,...>> =>
%                <<48,68,2,32,49,149,10,221,96,226,22,57,25,150,178,83,
%                  134,108,36,167,254,253,179,67,132,...>>},
%      <<"tx">> =>
%          [<<128,1,64,0,4,0,0,2>>,
%           1529940512033,1,<<"chainfee">>,
%           <<131,116,0,0,0,1,100,0,8,105,110,116,101,114,118,97,108,
%             97,10>>],
%      <<"type">> => "deploy"}

