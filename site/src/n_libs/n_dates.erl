-module(n_dates).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

datepicker(Id, Text) ->
	#datepicker_textbox {
		id=Id,
		text=Text,
		options=[
			{dateFormat, "yy-mm-dd"},
			{showButtonPanel, true}
		]
	}.

date_span(DateTime, N) ->
    StartRange = qdate:to_date(qdate:add_days(-N, DateTime)),
    EndRange = qdate:to_date(qdate:add_days(N, DateTime)),
    {StartRange, EndRange}.
