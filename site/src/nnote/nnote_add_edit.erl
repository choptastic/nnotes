%% -*- mode: nitrogen -*-
-module (nnote_add_edit).
-behavior(n_apps).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros
%% ***************************************************
-define(MMSELECTED, "nnote").
-define(TITLE, "Add/edit Note").
-define(TOP, "nnote").

url_vars() -> [id, note_type, task].

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
sidebar(#{note_type:=NoteType}) ->
    [
        #h3 {text="SELECT"},
        show_side_menu("NOTE TYPE", NoteType)
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
side_menu("NOTE TYPE") ->
    [{"conference",{select,"conference"}},
     {"idea", {select,"idea"}},
     {"interview", {select,"interview"}},
     {"lab", {select,"lab"}},
     {"lecture", {select,"lecture"}},
     {"research", {select,"research"}},
     {"web", {select,"web"}}
    ].

%% **************************************************
%% Sidebar events
%% **************************************************

event(search_by_tag) ->
    NoteType = wf:q(note_type),
    wf:update(content,
              content(#{note_type=>NoteType, task=>search_by_tag}));
%% ***************************************************
%% Info events
%% ***************************************************
event({info, Function}) ->
    wf:flash(info(Function));

event({select, NoteType}) ->
    Redirect = [wf:path(), "?",
                wf:to_qs([ {note_type, NoteType} ]) ],
    wf:redirect(Redirect);
event(search_by_date) ->
	NoteType = wf:q(note_type),
	Content = content(#{note_type=>NoteType, task=>search_by_date}),
	wf:update(content, Content).



%% ***************************************************
%% Info
%% ***************************************************
info(search_by_tag) ->
    [ #h2 {class=content, body =
           ["<i>Search Words</i>"]
          },
      #p {class=content, text=
          ["Search word documentation goes here"]
         }
    ];
info(search_by_date) ->
    [ #h2 {class=content, body =
           ["<i>Search Date</i>"]
          },
      #p {class=content, text=
          ["Search dates documentation goes here"]
         }
    ].


%% ***************************************************
%% Content executives
%% ***************************************************

content(#{id:=undefined, note_type:=undefined}) ->
    #h2{class=content, text="My Notes"};
content(#{id:=ID, note_type:=NoteType}) ->
    [
        content_headline(ID, NoteType),
        add_edit_form(ID, NoteType)
    ].

content_headline(ID, NoteType) ->
    Action = case ID of
        "new" -> "Enter";
        _ -> "Edit"
    end,
    #h2{class=content, text=[Action, " ",string:titlecase(NoteType)," Note"]}.

add_edit_form("new", NoteType) ->
    UserID = n_utils:get_user_id(),
    Date = qdate:to_string("m/d/Y"),
    form("new", UserID, Date, NoteType, "", "", "", "", "", "");
add_edit_form(ID, NoteType) ->
    %% We’ll do more here when we set up editing
    [].

form(ID, UserID, Date, NoteType, Event, Source, Topic,
     Question, Tags, Note) ->
    wf:defer(save_note, topic, #validate{validators=[
       #is_required{text="Topic required"}]}),
    wf:defer(save_note, note, #validate{validators=[
       #is_required{text="Note required"}]}),
    wf:defer(save_note, event, #validate{validators=[
       #is_required{text="Event required"}]}),
    wf:defer(save_note, source, #validate{validators=[
        #is_required{text="Source required"}]}),
    [ #label{text="Date"},
      #textbox{id=date, text=Date},
      #label{text="Type"},
      #textbox{id=type, text=NoteType},
      #label{text="Event"},
      #textbox{id=event, text=Event},
      #label{text=Source},
      #textbox{id=source, text=Source},
      #label{text="Topic"},
      #textbox{id=topic, text=Topic},
      #label{text="Question"},
      #textbox{id=question, text=Question},
      #label{text="Search Words"},
      #textbox{id=tags, text=Tags},
      #label{text="Note"},
      #textarea{id=note, text=Note},
      #br{},
      #button{id=save_note, text="Submit", postback={save_note}},
      #button{text="Cancel", postback=cancel}
    ].

button_text("new") -> "Enter new note";
button_text(_ID) -> "Submit changes".

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
	[#label {text="enter date"},
	 n_dates:datepicker(search, ""),
	 #button {text="Search", postback=search_by_date},
	 #button {text="Info", postback={info, search_by_date}}
	].

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
