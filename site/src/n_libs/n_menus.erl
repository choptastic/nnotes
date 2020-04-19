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
        {"Tips & Info", tips},
        {"Account", account}
    ].


%% ***************************************************
%% Main Menu Executive
%% ***************************************************
show_main_menu(Selected) ->
    MenuList = main_menu(),
    [show_main_menu_item(MenuItem, Selected) || MenuItem <- MenuList].

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

    #link {class=Class, text=Text, postback=Postback, delegate=?MODULE}.


if_selected(Text, Selected) ->
    case Text == Selected of
        true -> "mmselected" ;
        false -> "mm"
    end.

event(tips) ->
    Mod = wf:page_module(),
    wf:flash(Mod:tips());
event(account) ->
    wf:redirect_to_login("/login");
event(URL) ->
    wf:redirect(URL).


%% ***************************************************
%% Sidebar menus
%% ***************************************************
show_menu_item(MenuItem, Selected) ->
    {Text, Postback} = MenuItem,
    [#radio{ name=side_menu_item,
             text=Text,
             checked = (Text==wf:to_list(Selected)),
             value=Text,
             postback=Postback
           },
     #br {}
    ].

note_type_side_menu() ->
    [{"conference",{select,conference}},
     {"idea", {select,idea}},
     {"interview", {select,interview}},
     {"lab", {select,lab}},
     {"lecture", {select,lecture}},
     {"research", {select,research}},
     {"web", {select,web}}
    ].
