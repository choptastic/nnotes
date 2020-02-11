-module(n_apps).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

-callback main() -> body().

-callback url_vars() -> [atom()].

-callback main_menu() -> body().

-callback sidebar(map()) -> body().

-callback content(map()) -> body().
