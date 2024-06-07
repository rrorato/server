-module(room_actions).
-export([init/0, handle_action/3]).

init() ->
    ets:new(rooms, [named_table, public, bag]),
    ets:new(invitations, [named_table, public, bag]),
    ets:new(user_rooms, [named_table, public, bag]),
    ets:new(messages, [named_table, public, duplicate_bag]),
    ets:new(private_messages, [named_table, public, duplicate_bag]).

% room management
handle_action(Name, <<"createRoom">>, #{<<"room">> := Room, <<"access">> := Access}) ->
    ets:insert_new(rooms, {Room, Name, Access, []}),
    utils:build_dict("ok", "roomCreated");

handle_action(Name, <<"createRoom">>, #{<<"room">> := Room}) ->
    ets:insert_new(rooms, {Room, Name, <<"public">>, []}),
    utils:build_dict("ok", "roomCreated");

handle_action(Name, <<"destroyRoom">>, #{<<"room">> := Room}) ->
    case ets:lookup(rooms, Room) of
        [{Room, Name, _, _}] ->
            ets:delete(rooms, Room),
            ets:delete(user_rooms, {Room, Name}),
            utils:build_dict("ok", "roomDestroyed");
        _ ->
            utils:build_dict("error", "notAllowed")
    end;

handle_action(_Name, <<"listRooms">>, _) ->
    TabList = ets:tab2list(rooms),
    TabConcat = [Elem || {Elem, _, _, _} <- TabList],
    TabString = lists:flatten(io_lib:format("~p", [TabConcat])),
    TabBinary = iolist_to_binary(TabString),
    #{<<"state">> => <<"ok">>, <<"rooms">> => TabBinary};

handle_action(Name, <<"joinRoom">>, #{<<"room">> := Room}) ->
    case ets:lookup(rooms, Room) of
        [{Room, _, <<"public">>, _}] ->
            ets:insert(user_rooms, {Room, Name}),
            utils:build_dict("ok", "joinedRoom");
        [{Room, Name, _, _}] ->
            ets:insert(user_rooms, {Room, Name}),
            utils:build_dict("ok", "joinedRoom");
        _ ->
            utils:build_dict("error", "roomNotFound")
    end;

handle_action(Name, <<"leaveRoom">>, #{<<"room">> := Room}) ->
    ets:delete_object(user_rooms, {Room, Name}),
    utils:build_dict("ok", "leftRoom");

% messages in rooms
handle_action(_, <<"sendMessage">>, #{<<"room">> := Room, <<"message">> := Message}) ->
    case ets:lookup(rooms, Room) of
        [{Room, _, _, _}] ->
            ets:insert(messages, {Room, Message}),
            utils:build_dict("ok", "messageSent");
        [] ->
            utils:build_dict("error", "roomNotFound")
    end;

handle_action(Name, <<"receiveMessage">>, #{<<"room">> := Room}) ->
    case ets:lookup(rooms, Room) of
        [{Room, _, _, _}] ->
            case ets:match_object(user_rooms, {Room, Name}) of
                [{Room, Name}] ->
                    Messages = ets:match_object(messages, {Room, '_'}),
                    #{<<"state">> => <<"ok">>, <<"messages">> => lists:map(fun({_, Message}) -> Message end, Messages)};
                [] ->
                    utils:build_dict("error", "notInRoom")
            end;
        [] ->
            utils:build_dict("error", "roomNotFound")
    end;

% private messages
handle_action(Sender, <<"sendPrivateMessage">>, #{<<"receiver">> := Receiver, <<"message">> := Message}) ->
    ets:insert(private_messages, {Sender, Receiver, Message}),
    utils:build_dict("ok", "privateMessageSent");

handle_action(Receiver, <<"receivePrivateMessage">>, #{<<"sender">> := Sender}) ->
    Messages = ets:match_object(private_messages, {Sender, Receiver, '_'}),
    #{<<"state">> => <<"ok">>, <<"messages">> => lists:map(fun({_, _, Message}) -> Message end, Messages)};

% private rooms
handle_action(Name, <<"inviteInRoom">>, #{<<"room">> := Room, <<"user">> := User}) ->
    case ets:lookup(rooms, Room) of
        [{Room, Name, <<"private">>, _}] ->
            ets:insert(user_rooms, {Room, User}),
            utils:build_dict("ok", "inviteeJoinedRoom");
        [] ->
            utils:build_dict("error", "roomNotFound")
    end;

% other
handle_action(_Name, _Action, _ActionSpecs) ->
    io:format("~p taking unknown action ~p with options ~p ~n", [_Name, _Action, _ActionSpecs]),
    utils:build_dict("error", "unknownAction").