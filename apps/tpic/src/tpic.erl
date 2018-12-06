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

-module(tpic).

-export([cast/3]).
-export([call/3,call/4,cast_prepare/2]).
-export([peers/0,peers/1]).
-export([peer/1,peer/2]).

cast(TPIC, service, Message) ->
    gen_server:cast(TPIC, {broadcast, self(), service, Message});

cast(TPIC, Service, Message) when is_binary(Service) ->
    gen_server:cast(TPIC, {broadcast, self(), Service, Message});

cast(TPIC, Conn, Message) when is_tuple(Conn) ->
    gen_server:cast(TPIC, {unicast, self(), Conn, Message}).

call(TPIC, Conn, Request) -> 
    call(TPIC, Conn, Request, 2000).

call(TPIC, Service, Request, Timeout) when is_binary(Service) -> 
    R=gen_server:call(TPIC,{broadcast, self(), Service, Request}),
    T2=erlang:system_time(millisecond)+Timeout,
    lists:reverse(wait_response(T2,R,[]));

call(TPIC, Conn, Request, Timeout) when is_tuple(Conn) -> 
    R=gen_server:call(TPIC,{unicast, self(), Conn, Request}),
    T2=erlang:system_time(millisecond)+Timeout,
    lists:reverse(wait_response(T2,R,[])).

cast_prepare(TPIC,Service) when is_binary(Service) ->
    gen_server:call(TPIC, {broadcast, Service}).

wait_response(_Until,[],Acc) ->
    Acc;

wait_response(Until,[{_,_,R1x}=R1|RR],Acc) ->
    lager:debug("Waiting for reply"),
    T1=Until-erlang:system_time(millisecond),
    T=if(T1>0) -> T1;
        true -> 0
      end,
    receive 
        {'$gen_cast',{tpic,{_,_,R1x},A}} ->
            lager:debug("Got reply from ~p",[R1]),
            wait_response(Until,RR,[{R1,A}|Acc])
    after T ->
              wait_response(Until,RR,Acc)
    end.


peer(Handler) ->
	peer(tpic, Handler).

peer(TPIC, Handler) ->
	H=case Handler of
		   {A,_} -> {ass,A};
		   {A,_,_} -> {ass,A}
	   end,
	P=gen_server:call(TPIC, peers),
	case mkmap:get(H,P, undefined) of
		undefined -> undefined;
		#{state:=_}=W ->
			maps:with([state,authdata],W)
	end.

peers() ->
    peers(tpic).

peers(TPIC) ->
    MM=gen_server:call(TPIC, peers),
    lists:foldl(
      fun([Key|_]=AllKeys, Acc) ->
              Addresses=lists:foldl(
                          fun({ass,_},AAcc) -> AAcc;
                             ({{_,_,_,_,_,_,_,_}=Addr,Port},AAcc) ->
                                  [{inet:ntoa(Addr),Port}|AAcc]
                          end, [], AllKeys),
              Peer=maps:without([mych],mkmap:get(Key,MM)),
              Peer1=maps:put(addr,Addresses,Peer),
              [Peer1|Acc]
      end, [], mkmap:get_all_keys(MM)).


