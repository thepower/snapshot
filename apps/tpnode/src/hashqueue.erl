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

-module(hashqueue).
-export([new/0,
         add/4,
         remove/2,
         pop/1,
         get/2,
         head/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

new() ->
    {
     queue:new(),
     #{}
    }.

add(Key, Timestamp, Value, {Q, H}) ->
    {
     queue:in({Key, Timestamp}, Q),
     maps:put(Key, Value, H)
    }.

get(Key, {_Q, H}) ->
    maps:get(Key, H, undefined).

remove(Key, {Q, H}) ->
    {Q,
     maps:remove(Key, H)
    }.

head({Q, _H}) ->
    case queue:is_empty(Q) of
        true ->
            empty;
        false ->
            {_, T}=queue:head(Q),
            T
    end.

pop({Q, H}) ->
    case queue:out(Q) of
        {empty, Q2} ->
            {{Q2, #{}}, empty};
        {{value, {K, _}}, Q1} ->
            case maps:is_key(K, H) of
                true ->
                    {{Q1, maps:remove(K, H)}, {K, maps:get(K, H)}};
                false ->
                    pop({Q1, H})
            end
    end.

-ifdef(TEST).
default_test() ->
    QH0=new(),
    QH1=add(key1, 10, val1, QH0),
    QH2=add(key2, 10, val2, QH1),
    QH3=add(key3, 10, val3, QH2),
    QH4=add(key4, 13, val4, QH3),
    QH5=add(key5, 13, val5, QH4),
    {QH6, R1}=pop(QH5),
    {QH7, R2}=pop(QH6),
    QH8=remove(key3, QH7),
    QH9=remove(key4, QH8),
    {QH10, R3}=pop(QH9),
    [
    ?_assertEqual(empty, head(QH0)),
    ?_assertEqual(10, head(QH1)),
    ?_assertEqual(10, head(QH5)),
    ?_assertEqual({key1, val1}, R1),
    ?_assertEqual({key2, val2}, R2),
    ?_assertEqual(10, head(QH6)),
    ?_assertEqual(10, head(QH7)),
    ?_assertEqual({key5, val5}, R3),
    ?_assertEqual(empty, head(QH10)),
    ?_assertMatch(val4, get(key4, QH5)),
    ?_assertMatch(undefined, get(key4, QH10)),
    ?_assertMatch({_, empty}, pop(QH10))
    ].

-endif.

