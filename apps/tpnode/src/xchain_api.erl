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

-module(xchain_api).

%% API
-export([h/3, after_filter/1]).

after_filter(Req) ->
  Origin = cowboy_req:header(<<"origin">>, Req, <<"*">>),
  Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
                                    Origin, Req),
  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>,
                                    <<"GET, POST, OPTIONS">>, Req1),
  Req3 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>,
                                    <<"true">>, Req2),
  Req4 = cowboy_req:set_resp_header(<<"access-control-max-age">>,
                                    <<"86400">>, Req3),
  cowboy_req:set_resp_header(<<"access-control-allow-headers">>,
                             <<"content-type">>, Req4).

reply(Code, Result) ->
  EHF=fun([{Type, Str}|Tokens],{parser, State, Handler, Stack}, Conf) ->
          Conf1=jsx_config:list_to_config(Conf),
          if size(Str) == 32 ->
               jsx_parser:resume([{Type, <<"0x",(hex:encode(Str))/binary>>}|Tokens],
                                 State, Handler, Stack, Conf1);
             true ->
               jsx_parser:resume([{Type, base64:encode(Str)}|Tokens],
                                 State, Handler, Stack, Conf1)
          end
      end,
  {Code,
   {Result,
    #{jsx=>[ strict, {error_handler, EHF} ]}
   }
  }.

h(<<"GET">>, [<<"compat">>], _Req) ->
  reply(200,
        #{ ok => true,
           version => 2
         });

h(<<"POST">>, [<<"ping">>], _Req) ->
  reply(200,
        #{ ok => true,
           data => [<<"pong">>]
         });

h(<<"OPTIONS">>, _, _Req) ->
  {200, [], ""};

h(<<"GET">>, [<<"prev">>,BChain,<<"last">>], _Req) ->
  h(<<"GET">>, [<<"last">>,BChain], _Req);

h(<<"GET">>, [<<"prev">>,BChain,SBlkID], _Req) ->
  try
    Blk=case SBlkID of
             <<"0x", BArr/binary>> ->
               hex:parse(BArr);
             <<_:32/binary>> ->
               SBlkID;
             Any ->
               base64:decode(Any)
           end,
    Chain=binary_to_integer(BChain),
    Res=blockchain:rel(Blk,self),
    if is_map(Res) -> ok;
       is_atom(Res) ->
         throw({noblock, Res})
    end,
    O=maps:get(settings, Res),
    P=block:outward_ptrs(O,Chain),
    reply(200,
          #{ ok => true,
             chain=>blockchain:chain(),
             pointers => P
           })
  catch error:{badkey,outbound} ->
          reply(404,
                #{ ok=>false,
                   error => <<"no outbound">>
                 });
        throw:noout ->
          reply(404,
                #{ ok=>false,
                   error => <<"no outbound for this chain">>
                 });
        throw:{noblock, _R} ->
          reply(404,
                #{ ok=>false,
                   error => <<"no block">>
                 })
  end;

h(<<"GET">>, [<<"last">>,BChain], _Req) ->
  Chain=binary_to_integer(BChain),
  ChainPath=[<<"current">>, <<"outward">>, xchain:pack_chid(Chain)],
  Last=chainsettings:by_path(ChainPath),
  H=settings:get([<<".">>,<<"height">>,<<"ublk">>],Last),
  reply(200, #{ pointers=>maps:put(<<"hash">>,
                                   H,
                                   maps:remove(<<".">>,Last)
                                  ),
                chain=>blockchain:chain(),
                ok=>true });

h(<<"GET">>, [<<"owblock">>,BChain,SBlock], _Req) ->
  Block=case SBlock of
           <<"0x", BArr/binary>> ->
             hex:parse(BArr);
           <<_:32/binary>> ->
             SBlock;
           Any ->
             base64:decode(Any)
         end,
  Chain=binary_to_integer(BChain),
  Res=blockchain:rel(Block,self),
  OutwardBlock=block:outward_chain(Res,Chain),
  case OutwardBlock of
    none ->
      reply(404,
            #{ ok=>false,
               chain=>blockchain:chain(),
               block => false}
           );
    _AnyBlock ->
      reply(200,
            #{ ok => true,
               chain=>blockchain:chain(),
               block => block:pack(OutwardBlock),
               header => maps:with([hash, header, extdata],OutwardBlock)
             })
  end;

h(_Method, [<<"status">>], Req) ->
  {RemoteIP, _Port} = cowboy_req:peer(Req),
  lager:info("api call from ~p", [inet:ntoa(RemoteIP)]),
  Body = apixiom:body_data(Req),

  reply(200, #{
    ok=>true,
    data => #{
      request => Body
     }
   }).

