%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros
%% ***************************************************
-define(PATH, "/").
-define(TEMPLATE,"./site/templates/n_apps.html").
-define(MMSELECTED, "home").
-define(TITLE, "Welcome!").
-define(TOP, "Build it with Nitrogen").
-define(NICKNAME, n_utils:get_nickname()).

selected_menu() -> "home".

url_vars() -> [id, record_type, task].

%% ***************************************************
%% Template and Title
%% ***************************************************
main() -> #template { file=?TEMPLATE}.
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
content(#{}) ->
    [ #panel {id = content, body =
              [greeting()]
             }].

greeting() ->
    [#h2 {class=content, text=["Welcome to ", ?NICKNAME,
                               "’s ", "Nitrogen Applications!"]},
     #p {body = "Our motto: <em>\"Build it Fast with Nitrogen\"</em>"}
    ].

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

%% ***************************************************
%% Main menu events
%% ***************************************************
