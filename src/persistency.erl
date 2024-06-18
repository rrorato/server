-module(persistency).
-export([create_tables/0, save_state/0, read_state/0]).


create_tables() ->
    Tables = ["rooms", "invitations", "user_rooms", "messages", "private_messages"],
    lists:foreach(fun(Table) -> create_table(Table) end, Tables).

create_table(TableName) ->
    case table_exists(TableName) of
        true -> ok;
        false ->
            TableSchema = case TableName of
                "rooms" -> create_table_schema("rooms");
                "invitations" -> create_table_schema("invitations");
                "user_rooms" -> create_table_schema("user_rooms");
                "messages" -> create_table_schema("messages");
                "private_messages" -> create_table_schema("private_messages")
            end,
            dynamodb:create_table(TableSchema)
    end.

table_exists(TableName) ->
    case dynamodb:describe_table(TableName) of
        {ok, _} -> true;
        _ -> false
    end.

create_table_schema(TableName) ->
    % table schema with just id and string field "data"
    #{
        table_name => TableName,
        key_schema => [#{attribute_name => <<"id">>, key_type => hash}],
        attribute_definitions => [#{attribute_name => <<"id">>, attribute_type => string},
                                  #{attribute_name => <<"data">>, attribute_type => string}],
        provisioned_throughput => #{read_capacity_units => 1, write_capacity_units => 1}
    }.

save_state() ->
    % stringyfy ets tables and save them in dynamodb
    Tables = ["rooms", "invitations", "user_rooms", "messages", "private_messages"],
    lists:foreach(fun(Table) -> save_table(Table) end, Tables).

save_table(TableName) ->
    TableData = ets:tab2list(atom_to_list(TableName)),
    TableDataString = lists:flatten(io_lib:format("~p", [TableData])),
    TableDataBinary = iolist_to_binary(TableDataString),
    dynamodb:put_item(TableName, {<<"id">>, <<"data">>}, TableDataBinary).


read_state() ->
    % read tables from dynamodb and recreate ets tables
    Tables = ["rooms", "invitations", "user_rooms", "messages", "private_messages"],
    lists:foreach(fun(Table) -> read_table(Table) end, Tables).

read_table(TableName) ->
    case dynamodb:get_item(TableName, {<<"id">>, <<"data">>}) of
        {ok, TableDataBinary} ->
            TableDataString = binary_to_list(TableDataBinary),
            TableData = list_to_tuple(list_to_atom(TableDataString)),
            ets:new(TableName, [named_table, public, bag]),
            lists:foreach(fun(Elem) -> ets:insert(TableName, Elem) end, TableData);
        _ -> ok
    end.