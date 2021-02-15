-module(test_unit).
-export([test_all/0, test_everything/0]).  
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

test_everything() ->
    test_all().

testsuite() ->
	[{"unit test", spawn, [test_start(), test_default1(), test_default2(),
		test_add_mail(), test_enough(), test_get_config(), test_add_filter(),
		mytest_registered_filter(), mytest_add_filter_info(), mytest_add_filter_result()]}].

test_start() ->
	{
		"Start and Stop.",
		fun () ->
			{A, B} = mailfilter:start(infinite),
			?assertMatch({ok, _}, {A, B}),
			?assertEqual({ok, []}, mailfilter:stop(B))
		end
	}.

test_default1() ->
	{
		"Default. Simple filters.",
		fun () ->
			{ok, M} = mailfilter:start(infinite),
			?assertEqual(ok, mailfilter:default(M, t0, {simple, fun(_A, _B)->{just, 1} end}, [])),
			?assertEqual(ok, mailfilter:default(M, t1, {simple, fun(_A, _B)->{transformed, 2} end}, [])),
			?assertEqual(ok, mailfilter:default(M, t2, {simple, fun(_A, _B)->unchanged end}, [])),
			?assertEqual(ok, mailfilter:default(M, t3, {simple, fun(_A, _B)->{both, 3, 4} end}, [])),
			mailfilter:stop(M)
		end
	}.

test_default2() ->
	{
		"Default. Chain filters.",
		fun () ->
			{ok, M} = mailfilter:start(infinite),
			?assertEqual(ok, mailfilter:default(M, t0, {chain, [{simple, fun(_A, _B)->{just, 1} end}, {simple, fun(_A, _B)->{transformed, 2} end}]}, [])),
			?assertEqual(ok, mailfilter:default(M, t1, {chain, [{simple, fun(_A, _B)->{both, 3, 4} end}]}, [])),
			mailfilter:stop(M)
		end
	}.

test_add_mail()->
	{
		"Add mail. With or without default. When stop, check the length of mail state list.",
		fun () ->
			{ok, M} = mailfilter:start(infinite),
			?assertMatch({ok, _}, mailfilter:add_mail(M, "a")),
			mailfilter:default(M, t0, {simple, fun(_A, _B)->{just, 1} end}, []),
			?assertMatch({ok, _}, mailfilter:add_mail(M, "b")),
			{ok, S} = mailfilter:stop(M),
			?assertEqual(2, length(S))
		end
	}.

test_enough() ->
	{
		"Enough. Enough with MR in or not in. When stop, check the length of mail state list.",
		fun () ->
			{ok, M} = mailfilter:start(infinite),
			?assertEqual(ok, mailfilter:enough(1)),
			mailfilter:add_mail(M, "a"),
			mailfilter:add_mail(M, "a"),
			?assertEqual(ok, mailfilter:enough(0)),
			{ok, S} = mailfilter:stop(M),
			?assertEqual(1, length(S))
		end
	}.

test_get_config() ->
	{
		"Get config. When MR in or not in.",
		fun () ->
			{ok, M} = mailfilter:start(infinite),
			?assertMatch({error, _}, mailfilter:get_config(1)),
			mailfilter:add_mail(M, "a"),
			?assertMatch({ok, _}, mailfilter:get_config(0)),
			mailfilter:stop(M)
		end
	}.

test_add_filter() ->
	{
		"Add Filter. Simple and Chain. MR in and not in.",
		fun () ->
			{ok, M} = mailfilter:start(infinite),
			mailfilter:add_filter(0, t0, {simple, fun(_A, _B)->{just, 1} end}, []),
			mailfilter:add_mail(M, "a"),
			mailfilter:add_filter(0, t1, {simple, fun(_A, _B)->{just, 1} end}, []),
			mailfilter:add_filter(0, t2, {chain, [{simple, fun(_A, _B)->{both, 3, 4} end}]}, []),
			mailfilter:stop(M)
		end
	}.

mytest_registered_filter() ->
	{
		"Filters with one label would only register once, the latter ones would be discard.",
		fun () ->
			{ok, M} = mailfilter:start(infinite),
			mailfilter:default(M, t0, {simple, fun(_A, _B)->{just, 1} end}, []),
			mailfilter:default(M, t0, {simple, fun(_A, _B)->{transformed, 2} end}, []),
			mailfilter:add_mail(M, "a"),
			mailfilter:add_filter(0, t0, {simple, fun(_A, _B)->{just, 1} end}, []),
			{ok, {_C, F, _D}} = gen_server:call(M, get_all_state),
			?assertEqual(1, maps:size(F)),
			mailfilter:stop(M)
		end
	}.

mytest_add_filter_info() ->
	{
		"If MR not in or Label has been registered, return info.",
		fun () ->
			{ok, M} = mailfilter:start(infinite),
			?assertMatch({info, _}, mailfilter:add_filter(0, t0, {simple, fun(_A, _B)->{just, 1} end}, [])),
			mailfilter:add_mail(M, "a"),
			?assertEqual(ok, mailfilter:add_filter(0, t1, {simple, fun(_A, _B)->{just, 1} end}, [])),
			?assertMatch({info, _}, mailfilter:add_filter(0, t1, {simple, fun(_A, _B)->{just, 1} end}, [])),
			mailfilter:stop(M)
		end
	}.

mytest_add_filter_result() ->
	{
		"Check the result of the default and add filter functions.",
		fun () ->
			{ok, M} = mailfilter:start(infinite),
			mailfilter:add_filter(0, t2, {simple, fun(_A, _B)->{just, 1} end}, []),
			mailfilter:default(M, t1, {simple, fun(_A, _B)->{just, 1} end}, []),
			mailfilter:add_mail(M, "a"),
			mailfilter:add_filter(0, t0, {simple, fun(_A, _B)->{both, 3, 4} end}, []),
			{ok, {_C, F, _D}} = gen_server:call(M, get_all_state),
			?assertMatch([t0, t1], maps:keys(F)),
			timer:sleep(10),
			{ok, S} = mailfilter:stop(M),
			?assertEqual([{3,[{t1,{done,1}},{t0,{done,4}}]}], S)
		end
	}.