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

-module(tpnode_http).
-export([childspec/0, childspec_ssl/2, child_names_ssl/0, get_ssl_port/0, get_ssl_port/1]).


get_http_conn_type() ->
  HTTPDispatch = cowboy_router:compile(
    [
      {'_', [
        {"/api/ws", tpnode_ws, []},
        {"/api/[...]", apixiom, {tpnode_httpapi, #{}}},
        {"/[...]", cowboy_static,
          {dir, "public",
            [
              {mimetypes, cow_mimetypes, all},
              {dir_handler, directory_handler}
            ]
          }
        }
      ]}
    ]),
  #{
    connection_type => supervisor,
    env => #{
      dispatch => HTTPDispatch
    }
  }.


get_http_opts(Port) ->
  [
    {connection_type, supervisor},
    {port, Port}
  ].


childspec() ->
  Port = application:get_env(tpnode, rpcport, 43280),
  HTTPOpts = get_http_opts(Port),
  HTTPConnType = get_http_conn_type(),
  [
    ranch:child_spec(
      http,
      ranch_tcp,
      HTTPOpts,
      cowboy_clear,
      HTTPConnType
    ),
    ranch:child_spec(
      http6,
      ranch_tcp,
      [inet6, {ipv6_v6only, true} | HTTPOpts],
      cowboy_clear,
      HTTPConnType
    )
  ].


get_ssl_port() ->
  get_ssl_port(43380).

get_ssl_port(DefaultPort) ->
  application:get_env(tpnode, rpcsport, DefaultPort).
  

childspec_ssl(CertFile, KeyFile) ->
  Port = get_ssl_port(),
  SslOpts = [
    {certfile, utils:make_list(CertFile)},
    {keyfile, utils:make_list(KeyFile)}],
  HTTPOpts = get_http_opts(Port) ++ SslOpts,
  HTTPConnType = get_http_conn_type(),
  [
    ranch:child_spec(
      https,
      ranch_ssl,
      HTTPOpts,
      cowboy_clear,
      HTTPConnType
    ),
    ranch:child_spec(
      https6,
      ranch_ssl,
      [inet6, {ipv6_v6only, true} | HTTPOpts],
      cowboy_clear,
      HTTPConnType
    )
  ].


child_names_ssl() ->
  [https, https6].
