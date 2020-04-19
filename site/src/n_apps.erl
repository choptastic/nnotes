-module(n_apps).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

-optional_callbacks([access/0]).

-callback main() -> body().

-callback access() -> boolean().

-callback url_vars() -> [atom()].

-callback main_menu() -> body().

-callback sidebar(map()) -> body().

-callback content(map()) -> body().

