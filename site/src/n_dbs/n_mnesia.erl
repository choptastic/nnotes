-module(n_mnesia).
-export([init_tables/0,
         one_time/0,
         start/0,
         info/0,
         stop/0
        ]).

init_tables() ->
    erlias:build(nnote_db_mnesia, nnote_api),
    erlias:build(account_db_mnesia, account_api),
    nnote_db_mnesia:init_table(),
    account_db_mnesia:init_table().

one_time() ->
    schema(),
    init_tables().

schema() ->
    case mnesia:create_schema([node()]) of
        ok -> ok;
        {error, {_, {already_exists, _}}} -> ok;
        Other -> exit(Other)
    end,
    mnesia:start().

start() ->
    mnesia:start().

info() ->
    mnesia:info().

stop() ->
    mnesia:stop().


