-module (n_utils).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

get_nickname() -> "Marsha".

get_user_id() -> "123".

create_id() ->
    Rand = rand:uniform(1000000000),
    Seconds = qdate:unixtime(),
    GigaSeconds = Seconds * 1000000000,
    ID = GigaSeconds + Rand,
    integer_to_list(ID, 36).


draw_link(Record) ->
    ID = nnote_api:id(Record),
    NoteType = nnote_api:type(Record),
    DateTime = nnote_api:date(Record),
    Date = qdate:to_string("m/d/Y", DateTime),
    Topic = nnote_api:topic(Record),
    EditUrl = ["/nnote/add_edit?",
               wf:to_qs([{id, ID}, {note_type, NoteType}])],
    Menuid = wf:temp_id(),
    Wrapperid = wf:temp_id(),
    #panel{id=Wrapperid, body=[
        #link {
            body = [Date, " ", "&#8212;", " ", Topic],
            click=#toggle{target=Menuid}
        },
        #panel{id=Menuid, style="display:none", body=[
            #link {text="edit", url=EditUrl},
            " | ",
            #link {text="delete", postback={delete, ID, Wrapperid}}
        ]},
        #br{}
    ]}.

    

id_created(ID) ->
    IntID = list_to_integer(ID, 36),
    Seconds = IntID div 1000000000,
    qdate:to_date(Seconds).


field_names(FieldList) ->
    [atom_to_list(FieldName) || FieldName <- FieldList].

cap_field_names(FieldNames) ->
    [string:titlecase(String) || String <- FieldNames].

fieldnames_to_params(FieldNames) ->
    Caps = cap_field_names(FieldNames),
    string:join(Caps, ", ").

to_function_head(FieldList) ->
    FieldNames = field_names(FieldList),
    Params = fieldnames_to_params(FieldNames),
    lists:flatten(["populate_record([", Params, "]) ->"]).

synthesize_populate_record(Record, FieldList) ->
    Head = to_function_head(FieldList),
    Body = to_function_body(Record, FieldList),
    io:format("~s~n~s~n", [Head, Body]).

to_function_body(RecordName, FieldList) ->
    FieldNames = field_names(FieldList),
    Caps = cap_field_names(FieldNames),
    Zip = lists:zip(FieldNames, Caps),
    Assignments = lists:map(fun({Name, Cap}) ->
        Name ++ " = " ++ Cap
    end, Zip),
    Delimited = string:join(Assignments, ", "),
    lists:flatten(["   #", RecordName, "{", Delimited, "}."]).
