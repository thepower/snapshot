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

% -*- mode: erlang -*-
% vi: set ft=erlang :

-module(xchain_server_handler).

%% API
-export([handle_xchain/1, known_atoms/0]).

known_atoms() ->
  [iam, subscribed].

handle_xchain(#{null:=<<"last_ptr">>,
                <<"chain">>:=Chain}) ->
  ChainPath=[<<"current">>, <<"outward">>, xchain:pack_chid(Chain)],
  Last=chainsettings:get_settings_by_path(ChainPath),
  H=settings:get([<<".">>,<<"height">>,<<"ublk">>],Last),
  #{ null=><<"last_ptr">>,
     chain=>blockchain:chain(),
     pointers=>maps:put(<<"hash">>, H, maps:remove(<<".">>,Last)),
     ok=>true };

handle_xchain(#{null:=<<"pre_ptr">>,
                <<"chain">>:=Chain,
                <<"block">>:=Parent}) ->
  try
    Res=blockchain:rel(Parent,self),
    if is_map(Res) -> ok;
       is_atom(Res) ->
         throw({noblock, Res})
    end,
    O=maps:get(settings, Res),
    P=block:outward_ptrs(O,Chain),
    #{ ok => true,
       chain=>blockchain:chain(),
       null=><<"pre_ptr">>,
       pointers => P
     }
  catch 
    error:{badkey,outbound} ->
      #{ ok=>false,
         null=><<"pre_ptr">>,
         error => <<"no outbound">>
       };
    throw:noout ->
      #{ ok=>false,
         null=><<"pre_ptr">>,
         error => <<"no outbound for this chain">>
       };
    throw:{noblock, _R} ->
      #{ ok=>false,
         null=><<"pre_ptr">>,
         error => <<"no block">>
       };
    Ec:_ ->
      #{ ok=>false,
         null=><<"pre_ptr">>,
         error => Ec
       }
  end;

handle_xchain(#{null:=<<"owblock">>,
                <<"chain">>:=Chain,
                <<"parent">>:=Parent}) ->
  Res=blockchain:rel(Parent,self),
  OutwardBlock=block:outward_chain(Res,Chain),
  case OutwardBlock of
    none ->
      #{ ok=>false,
         null=><<"owblock">>,
         block => false};
    _AnyBlock ->
      #{ ok => true,
         chain=>blockchain:chain(),
         null=><<"owblock">>,
         block => block:pack(OutwardBlock),
         header => maps:map(
                     fun(extdata,PL) -> maps:from_list(PL);
                        (_,Val) -> Val
                     end,
                     maps:with([hash, header, extdata],OutwardBlock)
                    )
       }
  end;

handle_xchain(#{null:=<<"node_id">>,
                <<"node_id">>:=RemoteNodeId,
                <<"chain">>:=_RemoteChain}) ->
  try
    lager:info("Got nodeid ~p",[RemoteNodeId]),
    #{null=><<"iam">>, 
      <<"node_id">>=>nodekey:node_id(),
      <<"chain">>=>blockchain:chain()
     }
  catch _:_ ->
          error
  end;

handle_xchain(#{null:=<<"node_id">>,
                <<"node_id">>:=RemoteNodeId,
                <<"channels">>:=RemoteChannels}) ->
  try
    lager:info("Got nodeid ~p",[RemoteNodeId]),
    gen_server:cast(xchain_dispatcher,
                    {register_peer, self(), RemoteNodeId, RemoteChannels}),
    #{null=><<"iam">>, 
      <<"node_id">>=>nodekey:node_id(),
      <<"chain">>=>blockchain:chain()
     }
  catch _:_ ->
          error
  end;

handle_xchain(#{null:=<<"subscribe">>,
                <<"channel">>:=Channel}) ->
  gen_server:cast(xchain_dispatcher, {subscribe, Channel, self()}),
  {<<"subscribed">>, Channel};

handle_xchain(#{null:=<<"ping">>}) ->
  #{null=><<"pong">>};

handle_xchain(#{null:=<<"xdiscovery">>, <<"bin">>:=AnnounceBin}) ->
  gen_server:cast(discovery, {got_xchain_announce, AnnounceBin}),
  ok;

handle_xchain(ping) ->
  %%    lager:notice("got ping"),
  ok;

handle_xchain({node_id, RemoteNodeId, RemoteChannels}) ->
  try
    lager:info("Got old nodeid ~p",[RemoteNodeId]),
    gen_server:cast(xchain_dispatcher, {register_peer, self(), RemoteNodeId, RemoteChannels}),
    {<<"iam">>, nodekey:node_id()}
  catch _:_ ->
          error
  end;

handle_xchain(chain) ->
  try
    {ok, blockchain:chain()}
  catch _:_ ->
          error
  end;

handle_xchain(height) ->
  try
    {_, H} = gen_server:call(blockchain, last_block_height),
    {ok, H}
  catch _:_ ->
          error
  end;

handle_xchain({subscribe, Channel}) ->
  gen_server:cast(xchain_dispatcher, {subscribe, Channel, self()}),
  {<<"subscribed">>, Channel};

handle_xchain({xdiscovery, AnnounceBin}) ->
  gen_server:cast(discovery, {got_xchain_announce, AnnounceBin}),
  ok;

handle_xchain(Cmd) ->
  lager:info("xchain server got unhandled message from client: ~p", [Cmd]),
  {unhandled, Cmd}.

