-module(nodekey).

-export([get_priv/0,
         get_pub/0,
         node_id/0,
         node_id/1
        ]).

get_priv() ->
    {ok, K1}=application:get_env(tpnode, privkey),
    hex:parse(K1).

get_pub() ->
    tpecdsa:calc_pub(get_priv(), true).

node_id() ->
    node_id(get_pub()).

node_id(PubKey) ->
    Hash=crypto:hash(sha, PubKey),
    base58:encode(Hash).


