%% -*- mode: nitrogen -*-
-module(login).
-behavior(n_apps).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros
%% ***************************************************
-define(MMSELECTED, "home").
-define(TITLE, "Registration Page").
-define(TOP, "Build it with Nitrogen").

url_vars() -> [{task, atom}].

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

logged_in_msg(undefined) -> "Not Logged In";
logged_in_msg(Username) -> ["Logged In as ",Username].

sidebar(#{}) ->
    SignedOut = (n_utils:get_user_id()==undefined),
    Username = n_utils:get_nickname(),
    [
        #h2{text=logged_in_msg(Username)},
        #button{show_if=SignedOut, text="Create Account", postback={open,create}},
        #br{},
        #button{show_if=SignedOut, text="Sign In", postback={open,signin}},
        #br{},
        #button{show_if=not(SignedOut), text="Log Out", postback=logout}
    ].

event({open, Task}) ->
    UrlVars = #{task=>Task},
    wf:update(content, content(UrlVars));
event(logout) ->
    UrlVars = #{task=>undefined},
    wf:logout(),
    wf:update(content, content(UrlVars)),
    wf:update(sidebar, sidebar(UrlVars));
event(save) ->
    [Username, Email, Password] = wf:mq([username, email, password]),
    Record = account_api:new_account(Username, Email, Password),
    UserID = account_api:id(Record),
    wf:user(UserID),
    wf:session(username, Username),
    wf:redirect_from_login("/");
event(signin) ->
    [Username, Password] = wf:mq([username, password]),
    case account_api:attempt_login(Username, Password) of
        undefined ->
            wf:flash("No Matching Username or Password. Please Try Again");
        Record ->
            UserID = account_api:id(Record),
            wf:user(UserID),
            wf:session(username, Username),
            wf:redirect_from_login("/")
    end.

%% ***************************************************
%% Content executives
%% ***************************************************
content(#{task:=undefined}) ->
    #h3{text="Choose an option from the left"};
content(#{task:=create}) ->
    new_account_form();
content(#{task:=signin}) ->
    signin_form().

new_account_form() ->
    wf:defer(save, username, #validate{validators=[
        #is_required{text="Username Required"}]}),
    wf:defer(save, email, #validate{validators=[
        #is_required{text="Email Required"}]}),
    wf:defer(save, password, #validate{validators=[
        #is_required{text="Password Required"}]}),
    wf:defer(save, password2, #validate{validators=[
        #confirm_same{text="Passwords do not match",
            confirm_id=password}]}),
    [#h1{text="Create Account"},
     #label{text="Username"},
     #textbox{id=username, placeholder="Your Username"},
     #label{text="Email"},
     #textbox{id=email, placeholder="your@email.com"},
     #label{text="Password"},
     #password{id=password},
     #label{text="Confirm Password"},
     #password{id=password2},
     #br{},
     #button{id=save, text="Save Account", postback=save}
    ].


signin_form() ->
    wf:defer(signin, username, #validate{validators=[
        #is_required{text="Username"}]}),
    wf:defer(signin, password, #validate{validators=[
        #is_required{text="Password required"}]}),
    [
     #h1{body="Sign In"},
     #label{text="Username"},
     #textbox{id=username},
     #label{text="Password"},
     #password{id=password},
     #br{},
     #button{id=signin, text="Sign In", postback=signin}
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
