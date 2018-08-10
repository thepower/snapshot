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

-module(txgen).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, process/1, bals/0, is_running/0, restart/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

is_running() ->
	gen_server:call(?MODULE, status).

restart() ->
	gen_server:call(?MODULE, restart).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  Times=application:get_env(tpnode,txgen_times,2),
	{ok, #{
		ticktimer=>erlang:send_after(2000, self(), ticktimer),
		counter => Times,
		running => true}
	}.

handle_call(restart, _From, State) ->
  Times=application:get_env(tpnode,txgen_times,2),
	{reply, restarted, State#{
		ticktimer=>erlang:send_after(100, self(), ticktimer),
		counter => Times,
		running => true
	}};

handle_call(status, _From, #{ticktimer:=_Tmr, running := Running}=State) ->
	{reply, Running, State};

handle_call(_Request, _From, State) ->
    {reply, unknown, State}.

handle_cast(_Msg, State) ->
    lager:info("Unknown cast ~p", [_Msg]),
    {noreply, State}.

handle_info(ticktimer,
			#{ticktimer:=Tmr, counter := Counter}=State) ->
	catch erlang:cancel_timer(Tmr),
  TxsCnt=application:get_env(tpnode,txgen_txs,100),
  Delay=application:get_env(tpnode,txgen_int,6),
  lager:info("Starting TX generator ~w txs with ~w delay ~w more run(s)",
             [TxsCnt,Delay,Counter]),
	process(TxsCnt),
  if Counter > 0 ->
       {noreply, State#{
                   ticktimer=>erlang:send_after(Delay*1000, self(), ticktimer),
                   counter => Counter - 1
                  }
       };
     true ->
       {noreply, State#{running => false}}
  end;

handle_info(_Info, State) ->
    lager:info("Unknown info ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

bals() ->
	{ok, [L]}=file:consult("txgen.txt"),
	RS=lists:foldl(fun({Addr, _PKey}, Acc) ->
						   Ledger=ledger:get(Addr),
						   Bal=bal:get_cur(<<"FTT">>, Ledger),
						   [Bal|Acc]
				   end, [], L),
	{median(lists:sort(RS)), lists:sum(RS)/length(RS), lists:sum(RS), length(RS)}.

process(N) ->
	{ok, [L]}=file:consult("txgen.txt"),
	Addrs=lists:map(fun({Addr, _PKey}) -> Addr end, L),
	AL=length(Addrs),
	Prop=N/(AL*(AL-1)),
	PickAddr=fun(Addr0) ->
					 lists:map(
					   fun({_, A}) -> A end,
					   lists:sort(
						 lists:filtermap(fun(A0) when A0==Addr0 -> false;
											(A0) ->
												 {true, {rand:uniform(), A0}}
										 end, Addrs)
						)
					  )
			 end,
	L1=lists:foldl(fun({Addr, _PKey}, Acc) ->
						   Ledger=ledger:get(Addr),
						   Bal=bal:get_cur(<<"FTT">>, Ledger),
						   Seq0=bal:get(seq, Ledger),
						   if(Bal==0) -> Acc;
							 true ->
								 ToList=PickAddr(Addr),
								 TS=Bal/length(ToList),
								 {_, TR}=lists:foldl(
								   fun(To, {Seq, Acc1}) ->
										   R0=rand:uniform(),
										   Amount=round((rand:uniform(100)/100)*TS),
										   if(Amount>0 andalso R0<Prop) ->
												 Tx=#{
												   amount=>Amount,
												   cur=><<"FTT">>,
												   extradata=>jsx:encode(#{}),
												   from=>Addr,
												   to=>To,
												   seq=>Seq+1,
												   timestamp=>os:system_time(millisecond)
												  },
												 STX=tx:sign(Tx, _PKey),
												 {Seq+1, Acc1 ++ [
															   {{naddress:encode(Addr),
																 naddress:encode(To),
																 Amount},
																STX
																}]};
											 true ->
												 {Seq, Acc1}
										   end
								   end, {Seq0, []}, ToList),
								 Acc ++ TR
						   end
				   end, [], L),
	lists:map(
	  fun({T1, _TX}) ->
			  case txpool:new_tx(_TX) of
				  {ok, TXID} ->
					  {T1, TXID};
				  _ ->
					  {T1, error}
			  end
	  end, L1).

median([]) -> 0;
median([E]) -> E;
median(List) ->
    LL=length(List),
    DropL=(LL div 2)-1,
    {_, [M1, M2|_]}=lists:split(DropL, List),
    case LL rem 2 of
        0 -> %even elements
            (M1+M2)/2;
        1 -> %odd
            M2
    end.

