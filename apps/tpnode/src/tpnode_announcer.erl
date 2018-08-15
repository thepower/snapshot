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

-module(tpnode_announcer).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

-export([test/0, test1/0, test2/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    PeersCheckTimeout = maps:get(peers_check_timeout, Args, 10),
    register_node(),
    State = #{
        peers_check_timeout => PeersCheckTimeout,
        peers_check_timer => erlang:send_after(PeersCheckTimeout * 1000, self(), renew_peers)
    },
    {ok, State}.

handle_call(get_peers, _From, State) ->
    {reply, get_peers(), State};

handle_call(_Request, _From, State) ->
    lager:notice("Unknown call ~p", [_Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    lager:notice("Unknown cast ~p", [_Msg]),
    {noreply, State}.

handle_info(renew_peers, #{peers_check_timer:=PeersCheckTimer} = State) ->
    catch erlang:cancel_timer(PeersCheckTimer),
    #{peers_check_timeout := PeersCheckTimeout} = State,
    renew_peers(),
    {noreply, State#{
        peers_check_timer => erlang:send_after(PeersCheckTimeout * 1000, self(), renew_peers)
    }};

handle_info(_Info, State) ->
    lager:notice("Unknown info ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


parse_address(#{address:=Ip, port:=Port, proto:=Proto} = _Address) ->
    {Ip, Port, Proto};

parse_address(_Address) ->
    error.


get_local_addresses() ->
    gen_server:call(discovery, {get_config, addresses, []}).


filter_local_addresses(Peers) ->
    LocalAddresses = get_local_addresses(),
    LocalAddressFilter = fun({Ip, Port} = _Address) ->
        IsLocalAddress = lists:any(
            fun(#{address := LocalIp,
                port := LocalPort} = _LocalAddress) ->
                (
                  Ip == LocalIp andalso
                  Port == LocalPort
                );
                (InvalidLocalAddress) ->
                    lager:info("invalid local address, skip it: ~p",
															 [InvalidLocalAddress]),
										% use true to skip it by filter and avoid using
										% the invalid address in the renew_peers call
                    true
            end,
            LocalAddresses),
        (IsLocalAddress == false)
    end,
    lists:filter(LocalAddressFilter, Peers).


get_peers() ->
    AllNodes = gen_server:call(discovery, {lookup, <<"tpicpeer">>}),
    PeersFilter = fun(Address, Accepted) ->
        Parsed = parse_address(Address),
        case Parsed of
            {Ip, Port, tpic} ->
                Accepted ++ [{Ip, Port}];
            {Ip, Port, <<"tpic">>} ->
                Accepted ++ [{Ip, Port}];
            _ ->
                Accepted
        end
    end,
    AllPeers = lists:foldl(PeersFilter, [], AllNodes),
    filter_local_addresses(AllPeers).

renew_peers() ->
    renew_peers(get_peers()).

renew_peers([]) ->
    ok;
renew_peers(Peers) when is_list(Peers) ->
    lager:debug("add peers to tpic ~p", [Peers]),
    gen_server:call(tpic, {add_peers, Peers}).

register_node() ->
    gen_server:call(discovery, {register, <<"tpicpeer">>, self(), #{}}).


%% ---------------

test() ->
    register_node(),
    ok.

test1() ->
    get_peers().


test2() ->
    register_node(),
    LocalAddresses = get_local_addresses(),
    Peers = get_peers() ++ [{"127.0.0.1", 43214}],
    io:fwrite("local addresses: ~p~n", [LocalAddresses]),
    io:fwrite("peers: ~p~n", [Peers]),
    LocalAddressFilter = fun({Ip, Port} = _Address) ->
        IsLocalAddress = lists:any(
            fun(#{address := LocalIp,
                port := LocalPort,
                proto := Proto} = _LocalAddress) ->
                (Ip == LocalIp andalso
                 Port == LocalPort andalso
                 (Proto == tpic orelse Proto == <<"tpic">>)
                )
            end,
            LocalAddresses),
        (IsLocalAddress == false)
     end,
    NewPeers = lists:filter(LocalAddressFilter, Peers),
    io:fwrite("new peers: ~p~n", [NewPeers]).
