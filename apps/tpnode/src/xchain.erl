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

-module(xchain).

%% API
-export([pack/2, unpack/2, pack_chid/1, childspec/0]).


% -----------

pack(Term, 0) ->
    term_to_binary(Term);

pack(Atom, 2) when is_atom(Atom) ->
  pack(#{null=>atom_to_binary(Atom,utf8)},2);

pack({node_id, MyNodeId, Channels}, 2) ->
  pack(#{null=><<"node_id">>,
    <<"node_id">>=>MyNodeId,
    <<"channels">>=>Channels},2);

pack({subscribe, Channel}, 2) ->
  pack(#{null=><<"subscribe">>,
    <<"channel">>=>Channel},2);

pack({xdiscovery, Announce}, 2) ->
  pack(#{null=><<"xdiscovery">>, <<"bin">>=>Announce},2);

pack(Term, 2) when is_tuple(Term) ->
  pack(tuple_to_list(Term),2);

pack(Term, 2) ->
    msgpack:pack(Term).

% -----------

unpack(Bin,0) when is_binary(Bin) ->
  binary_to_term(Bin, [safe]);

unpack(Bin,2) when is_binary(Bin) ->
  {ok,Res}=msgpack:unpack(Bin),
  if is_list(Res) ->
       list_to_tuple(Res);
     true ->
       Res
  end;

unpack(Invalid, _) ->
    lager:info("xchain got invalid data for unpack ~p", [Invalid]),
    {}.

% -----------

pack_chid(I) when is_integer(I) ->
    <<"ch:", (integer_to_binary(I))/binary>>.

% -----------

childspec() ->
    HTTPDispatch = cowboy_router:compile(
        [
            {'_', [
                {"/xchain/ws", xchain_server, []},
                {"/", xchain_server, []}, %deprecated
                {"/xchain/api/[...]", apixiom, {xchain_api, #{}}}
            ]}
        ]),
    CrossChainOpts = application:get_env(tpnode, crosschain, #{}),
    CrossChainPort = maps:get(port, CrossChainOpts, 43311),


    HTTPOpts=[{connection_type, supervisor}, {port, CrossChainPort}],
    HTTPConnType=#{connection_type => supervisor,
        env => #{dispatch => HTTPDispatch}},
    [
        ranch:child_spec(crosschain_api,
            ranch_tcp,
            HTTPOpts,
            cowboy_clear,
            HTTPConnType),

        ranch:child_spec(crosschain_api6,
            ranch_tcp,
            [inet6, {ipv6_v6only, true}|HTTPOpts],
            cowboy_clear,
            HTTPConnType)
    ].

