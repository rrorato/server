-module(utils).
-export([build_dict/2]).


build_dict(State, Info) ->
    #{<<"state">> => list_to_binary(State), <<"info">> => list_to_binary(Info)}.