-module(account_db_mnesia).
-record(n_account, {
    id = n_utils:create_id(),
    username,
    email,
    date = qdate:unixtime(),
    pwhash
}).

-export([ init_table/0,
          put_record/1,
          get_all_values/1,
          get_all/0,
          get_record/1,
          delete/1,
          map_to_record/1,
          record_to_map/1,
          new_account/3,
          attempt_login/2,
          id/1,
          username/1,
          email/1,
          date/1,
          pwhash/1,
          id/2,
          username/2,
          email/2,
          date/2,
          pwhash/2
        ]).

-include_lib("stdlib/include/qlc.hrl").
-define(TABLE, n_account).

init_table() ->
    mnesia:create_table(?TABLE,
        [ {disc_copies, [node()] },
          {attributes,
           record_info(fields, ?TABLE)}
        ]).

put_record(Record) ->
    Insert = fun() ->
        mnesia:write(Record)
    end,
    {atomic, Results} = mnesia:transaction(Insert),
    Results.

get_all_values(Record) ->
    [_|Tail] = tuple_to_list(Record),
    Tail. 

get_all() ->
    Query = fun() ->
        qlc:eval( qlc:q(
            [ Record || Record <- mnesia:table(?TABLE) ]
        ))
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.

get_record(Key) ->
    Query = fun() ->
        mnesia:read({?TABLE, Key})
    end,
    {atomic, Results} = mnesia:transaction(Query),
    case length(Results) < 1 of
        true -> [];
        false -> hd(Results)
    end.

delete(Key) ->
    Insert = fun() ->
        mnesia:delete({?TABLE, Key})
    end,
    {atomic, Results} = mnesia:transaction(Insert),
    Results.

map_to_record(Map) ->
    n_utils:map_to_record(#n_account{}, record_info(fields, n_account), Map).

record_to_map(Record) ->
    n_utils:record_to_map(Record, record_info(fields, n_account)).

new_account(Username, Email, Password) ->
    PWHash = erlpass:hash(Password),
    Record = #n_account{username=Username,
                        email=Email,
                        pwhash=PWHash},
    put_record(Record),
    Record.

attempt_login(UserName, Password) ->
    Records = get_records_by_username(UserName),
    case Records of
        [] -> undefined;
        [Record] ->
            PWHash = Record#n_account.pwhash,
            case erlpass:match(Password, PWHash) of
                false -> undefined;
                true -> Record
            end
    end.

get_records_by_username(Username) ->
    Query = fun() ->
        qlc:eval( qlc:q(
                    [Record || Record <- mnesia:table(?TABLE),
                                         Record#n_account.username == Username]
        ))
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.

id(Record) ->
    Record#n_account.id.
username(Record) ->
    Record#n_account.username.
email(Record) ->
    Record#n_account.email.
date(Record) ->
    Record#n_account.date.
pwhash(Record) ->
    Record#n_account.pwhash.
id(Record, ID) ->
    Record#n_account{id=ID}.
username(Record, Username) ->
    Record#n_account{username=Username}.
email(Record, Email) ->
    Record#n_account{email=Email}.
date(Record, Date) ->
    Record#n_account{date=Date}.
pwhash(Record, PWHash) ->
    Record#n_account{pwhash=PWHash}.
