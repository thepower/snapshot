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

-module(xchain_client_handler).

%% API
-export([handle_xchain/3]).

handle_xchain(#{null:=<<"pong">>}, _ConnPid, Sub) ->
  Sub;

handle_xchain(#{null:=<<"iam">>, <<"node_id">>:=NodeId}, _ConnPid, Sub) ->
  Sub#{
    node_id => NodeId
   };

handle_xchain({iam, NodeId}, ConnPid, Sub) ->
    handle_xchain({<<"iam">>, NodeId}, ConnPid, Sub);

handle_xchain({<<"iam">>, NodeId}, _ConnPid, Sub) ->
  Sub#{
    node_id => NodeId
   };

handle_xchain(pong, _ConnPid, Sub) ->
%%    lager:info("Got pong for ~p", [_ConnPid]),
    Sub;

handle_xchain({<<"outward_block">>, FromChain, ToChain, BinBlock}, ConnPid, Sub) when
    is_integer(ToChain), is_integer(FromChain), is_binary(BinBlock) ->
  handle_xchain({outward_block, FromChain, ToChain, BinBlock}, ConnPid, Sub);

handle_xchain({outward_block, FromChain, ToChain, BinBlock}, _ConnPid, Sub) when
    is_integer(ToChain), is_integer(FromChain), is_binary(BinBlock) ->
    lager:info("Got outward block from ~b to ~b", [FromChain, ToChain]),
    Block=block:unpack(BinBlock),
    try
        Filename="tmp/inward_block." ++ integer_to_list(FromChain) ++ ".txt",
        file:write_file(Filename, io_lib:format("~p.~n", [Block]))
    catch Ec:Ee ->
        S=erlang:get_stacktrace(),
        lager:error("Can't dump inward block ~p:~p at ~p",
            [Ec, Ee, hd(S)])
    end,
    lager:debug("Here it is ~p", [Block]),
    gen_server:cast(txpool, {inbound_block, Block}),
    Sub;

handle_xchain({<<"subscribed">>,Cmd}, _ConnPid, Sub) ->
    lager:info("xchain client: subscribed successfully ~s", [Cmd]),
    Sub;

handle_xchain({<<"unhandled">>,Cmd}, _ConnPid, Sub) ->
    lager:info("xchain client: server did not understand my command: ~p", [Cmd]),
    Sub;

handle_xchain(Cmd, _ConnPid, Sub) ->
    lager:info("xchain client got unhandled message from server: ~p", [Cmd]),
    Sub.

