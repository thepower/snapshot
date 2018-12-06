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

-module(beacon).
-export([create/1, check/2, relay/2, parse_relayed/1]).

%% ------------------------------------------------------------------

create(To) ->
  Now = os:system_time(seconds),
  Priv = nodekey:get_priv(),
  create(To, Now, Priv).

create(To, Timestamp, Priv) when is_binary(To) ->
  Bin = <<Timestamp:64/big, To/binary>>,
  pack_and_sign(Bin, Priv).

%% ------------------------------------------------------------------

relay(To, Payload) when is_binary(To) andalso is_binary(Payload) ->
  Priv = nodekey:get_priv(),
  relay(To, Payload, Priv).

relay(To, Payload, Priv) ->
  Bin = <<16#BC, (size(To)):32/integer, To/binary, Payload/binary>>,
  pack_and_sign(Bin, Priv, 32).

%% ------------------------------------------------------------------

parse_relayed(<<16#BC, ToLen:32/integer, Rest/binary>>) ->
  <<To:ToLen/binary, Payload/binary>> = Rest,
  {To, Payload};

parse_relayed(<<16#BE, PayloadLen:32/integer, Rest/binary>> = Bin) ->
  <<PayloadBin:PayloadLen/binary, Sig/binary>> = Rest,
  HB = crypto:hash(sha256, PayloadBin),
  case bsig:checksig1(HB, Sig) of
    {true, #{extra:=Extra}} ->
      case parse_relayed(PayloadBin) of
        {To, Payload} ->
          Origin = proplists:get_value(pubkey, Extra),
          case chainsettings:is_our_node(Origin) of
            false ->
              error;
            _NodeName ->
              #{
                to => To,
                from => Origin,
                collection => Payload,
                bin => Bin
              }
          end;
        _ ->
          error
      end;
    false ->
      error
  end.

%% ------------------------------------------------------------------

pack_and_sign(Bin, Priv) ->
  pack_and_sign(Bin, Priv, 8).

pack_and_sign(Bin, Priv, SizeLen) when is_binary(Bin) andalso is_binary(Priv) ->
  HB = crypto:hash(sha256, Bin),
  Sig = bsig:signhash(HB, [], Priv),
  <<16#BE, (size(Bin)):SizeLen/integer, Bin/binary, Sig/binary>>.


%% ------------------------------------------------------------------

check(<<16#BE, PayloadLen:8/integer, Rest/binary>> = Bin, Validator) ->
  <<Payload:PayloadLen/binary, Sig/binary>> = Rest,
  <<Timestamp:64/big, Address/binary>> = Payload,
  HB = crypto:hash(sha256, Payload),
  case bsig:checksig1(HB, Sig) of
    {true, #{extra:=Extra}} ->
      Origin = proplists:get_value(pubkey, Extra),
      case chainsettings:is_our_node(Origin) of
        false ->
          error;
        _NodeName ->
          Beacon =
            #{
              to => Address,
              from => Origin,
              timestamp => Timestamp,
              bin => Bin
            },
          Validator(Beacon)
      end;
    false ->
      error
  end.

