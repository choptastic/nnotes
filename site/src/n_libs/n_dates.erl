-module(n_dates).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

datepicker(Id, Text) ->
	#datepicker_textbox {
		id=Id,
		text=Text,
		options=[
			{dateFormat, "mm-dd-yy"},
			{showButtonPanel, true}
		]
	}.

date_span(DateTime, N) ->
    StartRange = qdate:add_days(-N, DateTime),
    EndRange = qdate:add_days(N, DateTime),
    {StartRange, EndRange}.
