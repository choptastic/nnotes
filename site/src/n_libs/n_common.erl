-module(n_common).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

-define(PAGE, (wf:page_module())).
-define(TEMPLATE,"./site/templates/n_apps.html").

template() ->
    Access = get_access(),
    case can_access(Access) of
        true -> #template{file=?TEMPLATE};
        false -> wf:redirect_to_login("/login")
    end.

get_access() ->
    case erlang:function_exported(?PAGE, access, 0) of
        true -> ?PAGE:access();
        false -> public
    end.

can_access(public) ->
    true;
can_access(private) ->
    n_utils:get_user_id()=/=undefined.


get_page_vars() ->
    Vars = ?PAGE:url_vars(),
    lists:foldl(fun(Var, Map) ->
        {VarName, Value} = get_url_var(Var),
        maps:put(VarName, Value, Map)
    end, #{}, Vars).

get_url_var({Var, atom}) ->
    {Var, wf:to_existing_atom(wf:q(Var))};
get_url_var({Var, int}) ->
    {Var, wf:to_integer(wf:q(Var))};
get_url_var(Var) ->
    {Var, wf:q(Var)}.


main_menu() ->
    #panel{id=main_menu, body=?PAGE:main_menu()}.

sidebar() ->
    #panel{id=sidebar, body=?PAGE:sidebar(get_page_vars())}.

content() ->
    [
        #flash{},
        #panel{id=content, body=?PAGE:content(get_page_vars())}
    ].
