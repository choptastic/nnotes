-module(n_dates).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

datepicker(Id, Text) ->
	Text2 = ?WF_IF(Text=="", "", qdate:to_string("m/d/Y", Text)),

	#datepicker_textbox {
		id=Id,
		text=Text2,
		options=[
			{dateFormat, "mm/dd/yy"},
			{showButtonPanel, true}
		]
	}.

date_span(DateTime, N) ->
    StartRange = qdate:to_date(qdate:add_days(-N, DateTime)),
    EndRange = qdate:to_date(qdate:add_days(N, DateTime)),
    {StartRange, EndRange}.
