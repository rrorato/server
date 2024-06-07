-module(room_actions).
-export([init/0, handle_action/3]).

init() ->
    ok.

% other
handle_action(_Name, _Action, _ActionSpecs) ->
    io:format("~p taking unknown action ~p with options ~p ~n", [_Name, _Action, _ActionSpecs]),
    utils:build_dict("error", "unknownAction").