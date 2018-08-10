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

-module(bron_kerbosch).
-export([max_clique/1]).

-ifndef(TEST).
-define(TEST, 1).
-endif.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


is_connected(Row, Col, Matrix) ->
    lists:any(fun({Ind, List}) ->
									(Ind =:= Row)
									andalso
									lists:member(Col, List)
							end, Matrix)
        andalso
    lists:any(fun({Ind, List}) ->
									(Ind =:= Col)
									andalso
									lists:member(Row, List)
							end, Matrix).

check(Candidates, Rejected, Matrix) ->
    lists:foldl(fun(R, Acc1) ->
            Acc1 andalso lists:foldl(fun(C, Acc2) ->
                case is_connected(R, C, Matrix) of
                    true -> false;
                    false -> Acc2
                end
            end, true, Candidates)
        end, true, Rejected).

extend([], _, _, Results, _) -> Results;
extend(Candidates, Rejected, Compsub, Results, Matrix) ->
    case check(Candidates, Rejected, Matrix) of
        true ->
            [HCandidates|TCandidates] = Candidates,
            TRejected = case Rejected == [] of
                            true -> [];
                            false -> [_|TR] = Rejected, TR
                        end,

            NewCompsub = [HCandidates|Compsub],

						NewCandidates = lists:filter(
															fun(Val) ->
																	is_connected(Val, HCandidates, Matrix)
																	andalso
																	(Val =/= HCandidates)
															end, Candidates),

						NewRejected = lists:filter(
														fun(Val) ->
																is_connected(Val, HCandidates, Matrix)
																andalso
																(Val =/= HCandidates)
														end, Rejected),

            NewResults = case (NewCandidates == [])
															andalso
															(NewRejected == []) of
                             true -> [lists:sort(NewCompsub)|Results];
                             false -> extend(NewCandidates,
																						 NewRejected,
																						 NewCompsub,
																						 Results,
																						 Matrix)
                         end,
            extend(TCandidates, TRejected, Compsub, NewResults, Matrix);
        _ -> Results
    end.


max_clique(Matrix) ->
    Candidates = lists:sort(
									 lists:map(
										 fun({Ind, _}) ->
												 Ind
										 end, Matrix)),
    Result = lists:sort(extend(Candidates, [], [], [], Matrix)),
    lists:foldl(fun(Val, Acc) ->
        case length(Val) > length(Acc) of
            true -> Val;
            false -> Acc
        end
    end, [], Result).

-ifdef(TEST).
corrupt_matrix_test() ->
    Matrix = [{}],
    [
        ?assertError({badmatch, {}},
            max_clique(Matrix))
    ].

empty_matrix_test() ->
    Matrix = [],
    [
        ?assertEqual([],
            max_clique(Matrix))
    ].

integer_matrix_test() ->
    Matrix = [
        {0, [1, 2]},
        {1, [0, 2, 3, 4, 5, 6]},
        {2, [0, 1]},
        {3, [1, 4, 5, 6]},
        {4, [1, 3, 5, 6]},
        {5, [1, 3, 4, 6]},
        {6, [1, 3, 4, 5]}
    ],
    [
        ?assertEqual([1, 3, 4, 5, 6],
            max_clique(Matrix))
    ].

binary_matrix_test() ->
    Matrix = [
        {<<"a">>, [<<"A">>, <<"B">>]},
        {<<"A">>, [<<"a">>, <<"B">>, <<"C">>, <<"D">>, <<"E">>, <<"F">>]},
        {<<"B">>, [<<"a">>, <<"A">>]},
        {<<"C">>, [<<"A">>, <<"D">>, <<"E">>, <<"F">>]},
        {<<"D">>, [<<"A">>, <<"C">>, <<"E">>, <<"F">>]},
        {<<"E">>, [<<"A">>, <<"C">>, <<"D">>, <<"F">>]},
        {<<"F">>, [<<"A">>, <<"C">>, <<"D">>, <<"E">>]}
    ],
    [
        ?assertEqual([<<"A">>, <<"C">>, <<"D">>, <<"E">>, <<"F">>],
            max_clique(Matrix))
    ].

-endif.
