-module(main_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% -include_lib("ai_net/include/ai_net_types.hrl").
%% API
-compile(export_all).

init_per_suite(Config) ->
  %{ok, _} = application:ensure_all_started(random_list),
  Config.

end_per_suite(Config) ->
  Config.

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_TestCaseName, _Config) ->
  _Config.

all() -> [

].


