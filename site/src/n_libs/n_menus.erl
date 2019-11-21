-module(n_menus).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Main Menu
%% ***************************************************
main_menu() ->
    [   {"home", "/"},
        {"nindex", "/nindex"},
        {"nnote" , "/nnote"},
        {"Tips & Info", tips}
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

    %% Add an exercise:
    %% Using postbacks that are purely a redirects break expected user
    %% browser behavior, that's because middle-clicking a postback link
    %% doesn't open a new tab as a user expects.
    %% So the exercise is to rework the link generation here to use
    %% #link{url=URL} instead of a postback

    #link {class=Class, text=Text, delegate=?MODULE, postback=Postback}.


if_selected(Text, Selected) ->
    case Text == Selected of
        true -> "mmselected" ;
        false -> "mm"
    end.

event(tips) ->
    Mod = wf:page_module(),
    wf:flash(Mod:tips());
event(URL) ->
    wf:redirect(URL).


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
