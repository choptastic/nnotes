-module(n_search).
-compile(export_all).
%% ---------------------------------------------
%% @doc Search by keyword
%% ---------------------------------------------
unique_words_from_record(Record) ->
    Values = nnote_api:get_all_values(Record),
    List = [normalize(Value) || Value <- Values, is_list(Value)],
    sets:from_list(List).

unique_words_from_string(String) ->
    Normalized = normalize(String),
    Tokens = string:lexemes(Normalized, " "),
    sets:from_list(Tokens).

normalize(String) ->
    Clean = re:replace(String, "[,.?!()-]", "", [global,{return, list}]),
    string:to_upper(Clean).

filter(SearchString, Record) ->
    NoteSet = unique_words_from_record(Record),
    SearchSet = unique_words_from_string(SearchString),
    SharedWords = sets:intersection(NoteSet, SearchSet),
    sets:size(SharedWords) > 0.
