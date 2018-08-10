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

% -*- mode: erlang -*-
% vi: set ft=erlang :

-module(xchain_server).

%% API
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts, #{
        idle_timeout => 600000
    }}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({binary, Bin}, State) ->
    try
%%        lager:debug("ws server got binary msg: ~p", [Bin]),
        Cmd = xchain:unpack(Bin),
%%        lager:debug("ws server got term: ~p", [Cmd]),
        Result = xchain_server_handler:handle_xchain(Cmd),
        case Result of
            ok ->
                {ok, State};
            Answer ->
                {reply, {binary, xchain:pack(Answer)}, State}

        end
    catch
        Ec:Ee ->
            S = erlang:get_stacktrace(),
            lager:error("xchain server ws parse error ~p:~p ~p", [Ec, Ee, Bin]),
            lists:foreach(
                fun(Se) ->
                    lager:error("at ~p", [Se])
                end, S),
            {ok, State}
    end;

websocket_handle({text, <<"ping">>}, State) ->
    {ok, State};

websocket_handle({text, Msg}, State) ->
    lager:debug("xchain server got text msg: ~p", [Msg]),
    {reply, {text, <<"pong: ", Msg/binary >>}, State};

websocket_handle(_Data, State) ->
    lager:info("xchain server got unknown websocket: ~p", [_Data]),
    {ok, State}.

websocket_info({message, Msg}, State) ->
    lager:debug("xchain server send message ~p", [Msg]),
    {reply, {binary, xchain:pack(Msg)}, State};

websocket_info({timeout, _Ref, Msg}, State) ->
    lager:debug("xchain server ws timeout ~p", [Msg]),
    {reply, {text, Msg}, State};

websocket_info(_Info, State) ->
    lager:notice("xchain server got unknown ws info ~p", [_Info]),
    {ok, State}.


%% ----------------------------

