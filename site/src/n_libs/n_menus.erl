-module(n_menus).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Main Menu
%% ***************************************************
main_menu() ->
    [   {"home", {main, "/"}},
        {"nindex", {main, "/nindex"}},
        {"nnote" , {main, "/nnote"}},
        {"Tips & Info", {main, tips}}
    ].


%% ***************************************************
%% Main Menu Executive
%% ***************************************************
show_main_menu(Selected) ->
    MenuList = main_menu(),
    [
        #panel {id = main_menu, body =
            [show_main_menu_item(MenuItem, Selected) || MenuItem <- MenuList]
        }
    ].

%% ***************************************************
%% Main Menu Helpers
%% ***************************************************
show_main_menu_item(MenuItem, Selected) ->
    {Text, Postback} = MenuItem,
    Class = if_selected(Text, Selected),
    #link {class=Class, text=Text, postback=Postback}.


if_selected(Text, Selected) ->
    case Text == Selected of
        true -> "mmselected" ;
        false -> "mm"
    end.


%% ***************************************************
%% Sidebar menus
%% ***************************************************
show_menu_item(MenuItem, Selected) ->
    {Text, Postback} = MenuItem,
    [#radio{ name=Text,
             text=Text,
             checked =if_selected(Text, Selected),
             value=Text,
             postback=Postback
           },
     #br {}
    ].
