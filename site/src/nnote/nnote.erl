%% -*- mode: nitrogen -*-
-module (nnote).
-behavior(n_apps).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros
%% ***************************************************
-define(MMSELECTED, "nnote").
-define(TITLE, "Welcome to nnote!").
-define(TOP, "nnote").

url_vars() -> [id, record_type, task].

%% ***************************************************
%% Template and Title
%% ***************************************************
main() -> n_common:template().

title() -> ?TITLE.

%% ***************************************************
%% Panel definitions
%% ***************************************************

top() -> [ #h1 {text=?TOP} ].

main_menu() ->
    n_menus:show_main_menu(?MMSELECTED).

%% ***************************************************
%% Sidebar executives
%% ***************************************************
sidebar(#{}) ->
    [
        #h3 {text="SELECT"},
        show_side_menu("WEB SITE", unselected)
    ].

%% ***************************************************
%% Sidebar functions
%% ***************************************************
show_side_menu(Menu, Selected) ->
    [ #h4 {class=select, text=Menu},
      [n_menus:show_menu_item(MenuItem, Selected) ||
       MenuItem <- side_menu(Menu)]
    ].

%% ***************************************************
%% Sidebar menus
%% ***************************************************
side_menu("WEB SITE") ->
    [{"nitrogen", {goto,
                   "http://nitrogenproject.com/"}},
     {"erlang", {goto,
                 "http://erlang.org/doc/apps/stdlib/"}},
     {"hacker news", {goto,
                      "https://news.ycombinator.com/"}}
    ].

%% ***************************************************
%% Content executives
%% ***************************************************
content(#{note_type:=undefined, task:=undefined}) ->
    [content_headline(),
     #p{class=content, text="Select note type."}
    ];
content(#{note_type:=NoteType, task:=Task}) ->
    Records = case Task of
        undefined -> undefined;
        search_by_tag -> tag_search(NoteType);
        date_search -> date_search(NoteType)
    end,
    display_forms(NoteType, Records).

content_headline() ->
    [#h2 {class=content, text="My Notes"}].

%% ***************************************************
%% Content
%% ***************************************************
display_forms(NoteType, Records) ->
    [ #panel {id = content, body =
      [content_headline(),
       add_note_button(NoteType),
       search_by_tag(),
       search_by_date(),
       search_results(Records)
      ]}].

search_results(undefined) ->
    [];
search_results([]) ->
    [#hr {},
     #h2 {class = content, text="Search Results"},
     #p {text = "No notes found"}
    ];
search_results(Records) ->
    [#hr {},
     #h2 {class = content, text="Search Results"},
     [n_utils:draw_link(Record) || Record <- Records]
    ].


add_note_button(NoteType) ->
    ButtonText = ["Enter new ",NoteType," note"],
    #button {text=ButtonText, postback={add_note, NoteType}}.
search_by_tag() ->
    [#label {text="enter search words"},
     #textbox {id = search_words},
     #button {text="Search", postback=search_by_tag},
     #button {text="Info", postback={info, search_by_tag}}
    ].
search_by_date() ->
    io:format("Search by date~n").


tag_search(NoteType) ->
    [].

date_search(NoteType) ->
    [].

%% ***************************************************
%% Tips
%% ***************************************************
tips() ->
    [
        #h2 {class="content", text="Tips & Info"},
        #p {body="The applications in this framework
            were developed by Jesse Gumm and
            Lloyd R. Prentice for their book
          <em>Build it with Nitrogen</em>. These
          applications are available for use and
          modification under the MIT License."}
    ].
