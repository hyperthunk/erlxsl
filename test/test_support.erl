-module(test_support).
-compile(export_all).

get_fixture_dir(DataDir) ->
    SuiteData = filename:split(DataDir),
    TestDir = filename:join(lists:sublist(SuiteData, length(SuiteData) - 1)),
    filename:join(TestDir, "fixtures").
