-module(n_common).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

-define(PAGE, (wf:page_module())).

get_page_vars() ->
    Vars = ?PAGE:url_vars(),
    wf:q_map(Vars).

main_menu() ->
    #panel{id=main_menu, body=?PAGE:main_menu()}.

sidebar() ->
    #panel{id=sidebar, body=?PAGE:sidebar(get_page_vars())}.

content() ->
    [
        #flash{},
        #panel{id=content, body=?PAGE:content(get_page_vars())}
    ].
