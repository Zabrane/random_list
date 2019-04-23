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
  test_1,
  test_2
].

test_1(_Config) ->
  List = [ 1,2,3 ],
  R = random_list:new(List),
  { ok, A, R0 } = random_list:pop(R),
  ?assertEqual(true, lists:member(A, List)),
  { ok, B, R1 } = random_list:pop(R0),
  ?assertEqual(true, lists:member(B, List -- [ A ]) ),
  { ok, C, R2 } = random_list:pop(R1),
  ?assertEqual(true, lists:member(C, List -- [ A, B ]) ),
  ?assertEqual(true, random_list:is_empty(R2)),
  ok.

test_2(_Config) ->
  List = [ 1,2,3 ],
  R = random_list:new(List),
  Res0 = random_list:fold(fun(Item, Acc) -> [ Item | Acc ] end, [], R),
  ?assertEqual([], Res0 -- List),
  Res1 = random_list:map(fun(Item) -> Item end, R),
  ?assertEqual([], Res1 -- List),
  ok.
