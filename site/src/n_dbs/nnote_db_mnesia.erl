-module(nnote_db_mnesia).
-record(nnote, {
    id = n_utils:create_id(),
    user_id,
    type,
    date,
    event,
    source,
    topic,
    question,
    tags,
    note
}).

-export([ init_table/0,
          put_record/1,
          get_all_values/1,
          get_all/0,
          get_record/1,
          delete/1,
          populate_record/1,
          get_records_by_type/2,
          get_records_by_date/3,
          search/3,
          id/1,
          user_id/1,
          date/1,
          type/1,
          event/1,
          source/1,
          topic/1,
          question/1,
          tags/1,
          note/1,
          id/2,
          user_id/2,
          date/2,
          type/2,
          event/2,
          source/2,
          topic/2,
          question/2,
          tags/2,
          note/2
        ]).

-include_lib("stdlib/include/qlc.hrl").
-define(TABLE, nnote).

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

populate_record([User_id, Type, Date, Event, Source,
                 Topic, Question, Tags, Note]) ->
    #nnote{
           user_id = User_id,
           date = Date,
           type = Type,
           event = Event,
           source = Source,
           topic = Topic,
           question = Question,
           tags = Tags,
           note = Note
          }.

get_records_by_type(UserID, Type) ->
    Query =
    fun() ->
            qlc:eval( qlc:q(
                        [Record || Record <- mnesia:table(?TABLE),
                                   Record#nnote.id == UserID,
                                   Record#nnote.type == Type]
                       ))
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.

get_records_by_date(UserID, Type, Date) ->
    DateTime = qdate:to_date(Date),
    {FirstDate, LastDate} = n_dates:date_span(Date, 7),
    wf:info("~p=~p",[Date, {UserID, Type, FirstDate, LastDate}]),
    Query = fun() ->
            qlc:eval( qlc:q(
                        [Record || Record <- mnesia:table(?TABLE),
                                   between(FirstDate, Record#nnote.date, LastDate)==true,
                                   Record#nnote.user_id == UserID,
                                   Record#nnote.type == Type
                        ]))
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.

between(S, X, E) ->
    wf:info("~p =< ~p =< ~p",[qdate:to_string("Y-m-d", S), X,  qdate:to_string("Y-m-d", E)]),
    qdate:between(S, X, E).

search(UserID, NoteType, undefined) -> [];
search(UserID, NoteType, SearchList) ->
    Query = fun() ->
            qlc:eval( qlc:q(
                        [Record || Record <- mnesia:table(?TABLE),
                                   Record#nnote.user_id == UserID,
                                   Record#nnote.type == NoteType,
                                   n_search:filter(SearchList, Record)]
                       ))
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.



id(Record) -> Record#nnote.id.
user_id(Record) -> Record#nnote.user_id.
date(Record) -> Record#nnote.date.
type(Record) -> Record#nnote.type.
event(Record) -> Record#nnote.event.
source(Record) -> Record#nnote.source.
topic(Record) -> Record#nnote.topic.
question(Record) -> Record#nnote.question.
tags(Record) -> Record#nnote.tags.
note(Record) -> Record#nnote.note.

%% SETTERS
id(Record, ID) ->
    Record#nnote{id=ID}.
user_id(Record, UserID) ->
    Record#nnote{user_id=UserID}.
date(Record, Date) ->
    Record#nnote{date=Date}.
type(Record, Type) ->
    Record#nnote{type=Type}.
event(Record, Event) ->
    Record#nnote{event=Event}.
source(Record, Source) ->
    Record#nnote{source=Source}.
topic(Record, Topic) ->
    Record#nnote{topic=Topic}.
question(Record, Question) ->
    Record#nnote{question=Question}.
tags(Record, Tags) ->
    Record#nnote{tags=Tags}.
note(Record, Note) ->
    Record#nnote{note=Note}.
