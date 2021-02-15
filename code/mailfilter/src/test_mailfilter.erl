-module(test_mailfilter).

-include_lib("eqc/include/eqc.hrl").

-export([test_all/0, test_everything/0]).
-export([wellbehaved_filter_fun/0, filter/1, prop_mail_is_sacred/0, prop_consistency/0]). % Remember to export the other function from Q2.2


% You are allowed to split your test code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_mailfilter.


test_all() ->
	eqc:module(test_mailfilter).

test_everything() ->
    test_all().

wellbehaved_filter_fun() -> 
	I1 = int(),
	I2 = int(),
	I3 = int(),
	I4 = int(),
    oneof([fun(_Mail, _Data) -> {just, I1} end,
    	fun(_Mail, _Data) -> {transformed, I2} end,
    	fun(_Mail, _Data) -> unchanged end,
    	fun(_Mail, _Data) -> {both, I3, I4} end]).

filter(FunGen) ->
	oneof([{chain, ?SUCHTHAT(L, list(FunGen()), length(L)>0)}, {simple, FunGen()}]).

prop_mail_is_sacred() ->
	{ok, M} = mailfilter:start(infinite),
	{ok, MR1} = mailfilter:add_mail(M, "a"),
	{ok, MR2} = mailfilter:add_mail(M, "b"),
	mailfilter:enough(MR1),
	{ok, MR3} = mailfilter:add_mail(M, "c"),
	mailfilter:enough(MR3),
	mailfilter:enough(MR3),
	{ok, Res} = mailfilter:stop(M),
	length(Res) == 1.

prop_consistency() ->
	{ok, M} = mailfilter:start(infinite),
	mailfilter:default(M, t0, {simple, fun(_A, _B)->{transformed, 0} end}, []),
	{ok, MR} = mailfilter:add_mail(M, "a"),
	mailfilter:add_filter(MR, t1, {simple, fun(_A, _B)->{transformed, 1} end}, []),
	timer:sleep(10),
	{ok, Res} = mailfilter:stop(M),
	if Res == [{0,[{t0,{done,nothing}},{t1,inprogress}]}] -> true;
		true ->
			if Res == [{1,[{t0,inprogress},{t1,{done,nothing}}]}] -> true;
				true -> false
			end
	end.


prop_filter() ->
	?FORALL(Filter, filter(fun wellbehaved_filter_fun/0),
		begin
			{ok, M} = mailfilter:start(infinite),
			mailfilter:default(M, t0, Filter, []),
			{ok, MR} = mailfilter:add_mail(M, "aa"),
			mailfilter:add_filter(MR, t1, Filter, []),
			timer:sleep(10),
			{ok, Res} = mailfilter:stop(M),
			collect(Res, true)
		end
	).