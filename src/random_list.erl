-module(random_list).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-compile({no_auto_import, [ size/1, get/1 ]}).

%% API
-export([
  new/0, new/1, new/2,
  size/1, is_empty/1,
  push/2, merge/2,
  pop/1, get/1, pop_push/1, remove/2,
  to_list/1,
  fold/3, foreach/2, shuffle/1,
  map/2, filter/2, partition/2
]).


-define(MINIMAL_LIST, 5). %% Not separated minimal list

-record(rl_pair, {
  left,
  right
}).

-record(random_list, {
  size = 0 :: pos_integer(),
  items = [] :: [] | #rl_pair{}
}).

-type random_list() :: #random_list{}.

-export_type([
  random_list/0
]).

new() -> #random_list{}.

new(List) -> new(length(List), List).

new(Size, List) ->
  #random_list{
    size = Size,
    items = List
  }.

size(List) ->
  List#random_list.size.

is_empty(List) ->
  size(List) =:= 0.

push(List = #random_list{ items = L }, Item) when is_list(L) ->
  #random_list{
    size = List#random_list.size + 1,
    items = [ Item | L ]
  };

push(List = #random_list{ items = #rl_pair{ left = Left, right = Right } = Items}, Item) ->
  NewItems = case size(Left) =< size(Right) of
    true ->
      Items#rl_pair{
        left = push(Left, Item)
      };
    false ->
      Items#rl_pair{
        right = push(Right, Item)
      }
  end,
  List#random_list{
    size = List#random_list.size + 1,
    items = NewItems
  }.

pop(#random_list{ size = 0 }) -> { error, list_is_empty };
pop(#random_list{ size = 1, items = [Item]}) ->
  { ok, Item, new() };

pop(List) ->
  Position = rand:uniform(size(List)),
  pop(Position, List).

pop_push(List) ->
  case pop(List) of
    { ok, Item, NewList } -> { ok, Item, push(NewList, Item) };
    Any -> Any
  end.

get(#random_list{ size = 0 }) -> {error, list_is_empty };
get(List) ->
  Position = rand:uniform(size(List)),
  get(Position, List).

to_list(#random_list{ size = 0 }) -> [];
to_list(#random_list{ items = List }) when is_list(List) -> List;
to_list(#random_list{ items = #rl_pair{ left = Left, right = Right } }) ->
  SizeLeft = size(Left),
  SizeRight = size(Right),
  if
    SizeLeft =< SizeRight -> to_list(Left) ++ to_list(Right);
    true -> to_list(Right) ++ to_list(Left)
  end.

merge(R1 = #random_list{ items = List1, size = Size1 }, R2 = #random_list{ items = List2, size = _Size2 }) when is_list(List1), is_list(List2), Size1 < ?MINIMAL_LIST ->
  #random_list{
    size = size(R1) + size(R2),
    items = List1 ++ List2
  };

merge(R1 = #random_list{ items = List1, size = Size1 }, R2 = #random_list{ items = List2, size = Size2 }) when is_list(List1), is_list(List2), Size2 < ?MINIMAL_LIST, Size1 >= ?MINIMAL_LIST ->
  merge(R2, R1);

merge(R1 = #random_list{ items = List1 }, R2 = #random_list{ items = List2 }) when is_list(List1), is_list(List2) ->
  Items = case size(R1) > size(R2) of
    true -> simplify(R1, R2);
    false -> simplify(R2, R1)
  end,
  #random_list{
    size = size(R1) + size(R2),
    items = Items
  };

merge(R1 = #random_list{ items = #rl_pair{ left = Left1, right = Right1 } }, R2 = #random_list{ items = #rl_pair{ left = Left2, right = Right2 }}) ->
  #random_list{
    size = size(R1) + size(R2),
    items = simplify(merge(Left1, Left2),merge(Right1, Right2))
  };

merge(R1 = #random_list{ items = #rl_pair{ left = Left1, right = Right1 } }, R2 = #random_list{ items = _List2 }) ->
  D = size(R2),
  NewItems = case size(Left1) + D =< size(Right1) of
    true -> simplify(merge(Left1, R2), Right1);
    false -> simplify(Left1, merge(Right1, R2))
  end,
  #random_list{
    size = size(R1) + size(R2),
    items = NewItems
  };

merge(R1 = #random_list{ items = List1 }, R2 = #random_list{ items = #rl_pair{} }) when is_list(List1) ->
  merge(R2, R1).

%% High order functions

map(Fun, RandomList = #random_list{ items = List }) when is_list(List) ->
  NewItems = lists:map(Fun, List),
  RandomList#random_list{
    items = NewItems
  };

map(Fun, RandomList = #random_list{ items = #rl_pair{ left = Left, right = Right } }) ->
  RandomList#random_list{
    items = #rl_pair{
      left = map(Fun, Left),
      right = map(Fun, Right)
    }
  }.

filter(Fun, RandomList = #random_list{ items = List }) when is_list(List) ->
  NewItems = lists:filter(Fun, List),
  RandomList#random_list{
    size = length(NewItems),
    items = NewItems
  };

filter(Fun, RandomList = #random_list{ items = #rl_pair{ left = Left, right = Right } }) ->
  NewLeft = filter(Fun, Left),
  NewRight = filter(Fun, Right),
  case { size(NewLeft), size(NewRight) } of
    { 0,0  } -> new(0, []);
    { 0, _ } -> NewRight;
    { _, 0 } -> NewLeft;
    { A, B } when A+B =< ?MINIMAL_LIST ->
      RandomList#random_list{
        size = A + B,
        items = to_list(NewLeft) ++ to_list(NewRight)
      };
    { A, B } when B > A ->
      RandomList#random_list{
        size = A + B,
        items = #rl_pair{
          left = NewRight,
          right = NewLeft
        }
      };
    { A, B } when A >= B ->
      RandomList#random_list{
        size = A + B,
        items = #rl_pair{
          left = NewLeft,
          right = NewRight
        }
      }
  end.

fold(Fun, Acc, List) ->
  case is_empty(List) of
    true -> Acc;
    false ->
      { ok, Item, NewList} = pop(List),
      fold(Fun, Fun(Item, Acc), NewList)
  end.

foreach(Fun, List) ->
  fold(fun(Item, _) ->
    Fun(Item),
    ok
  end, ok, List).

partition(Fun, #random_list{ items = List }) when is_list(List) ->
  {Satisfying, NotSatisfying} = lists:partition(Fun, List),
  {new(Satisfying), new(NotSatisfying)};

partition(Fun, #random_list{ items = #rl_pair{ left = Left, right = Right } }) ->
  {SatisfyingLeft, NotSatisfyingLeft} = filter(Fun, Left),
  {SatisfyingRight, NotSatisfyingRight} = filter(Fun, Right),
  { merge(SatisfyingLeft, SatisfyingRight), merge(NotSatisfyingLeft, NotSatisfyingRight) }.

shuffle(List) ->
  fold(fun(Item, Acc) -> [ Item | Acc ] end, [], List).

remove(R = #random_list{ items = List }, Element) when is_list(List) ->
  R0 = lists:search(fun(V) -> Element =:= V end, List),
  case R0 of
    false -> { not_found, R};
    { value, Item} ->
      NewR = R#random_list{
        items = List -- [Item],
        size = size(R) - 1
      },
      { changed, NewR }
  end;

remove(R = #random_list{ items = #rl_pair{ left = Left, right = Right}}, Element) ->
  Result = case remove(Left, Element) of
    { changed, NLeft} -> { changed, NLeft, Right };
    { not_found, _ } ->
      case remove(Right, Element) of
        { changed, NRight} -> { changed, Left, NRight};
        { not_found, _ } ->  not_found
      end
  end,
  case Result of
    { changed, NewLeft, NewRight } ->
      NewR = R#random_list{
        items = simplify(NewLeft, NewRight),
        size = size(R) - 1
      },
      { changed, NewR };
    not_found -> { not_found, R }
  end.

%% Internal

pop(Position, #random_list{ size = Size }) when Position > Size -> { error, bad_position };
pop(Position, List) ->
  #random_list{
    size = Size,
    items = Items
  } = List,
  { Item, NewSize, NewItems } = extract(Position, Size, Items),
  { ok, Item, #random_list{
    size = NewSize,
    items = NewItems
  }}.

get(_, #random_list{ size = 0 }) -> {error, list_is_empty };
get(Number, #random_list{ items = List }) when is_list(List) ->
  {ok, lists:nth(Number, List)};
get(Number, #random_list{ items = #rl_pair{ left = Left, right = Right } }) ->
  Delta = size(Left) - Number,
  case Delta >= 0 of
    true -> get(Delta, Left);
    false -> get(-Delta, Right)
  end.

extract(Position, Size, List) when is_list(List), Size < ?MINIMAL_LIST ->
  El = lists:nth(Position, List),
  { El, Size - 1, List -- [El]};

extract(Position, Size, List) when is_list(List) ->
  { Item, NewList } = extract(Position, List, Size, [], 0),
  { Item, Size - 1, NewList };

extract(Position, Size, #rl_pair{ left = Left, right = Right}) ->
  SizeLeft = size(Left),
  case  SizeLeft < Position of
    true ->
      { Item, NewSize, NewList } = extract(Position - SizeLeft, size(Right), Right#random_list.items),
      { Item, Size - 1, simplify(Left, new(NewSize, NewList)) };
    false ->
      { Item, NewSize, NewList } = extract(Position, SizeLeft, Left#random_list.items),
      { Item, Size - 1, simplify(new(NewSize, NewList), Right) }
  end.

extract(1, [ Item | Left ], SizeLeft, Right, SizeRight ) ->
  NewItems = if
    SizeLeft =< SizeRight -> #rl_pair{
      left = new(SizeLeft - 1 , Left),
      right = new(SizeRight, Right)
    };
    true -> #rl_pair{
      left = new(SizeRight, Right),
      right = new(SizeLeft - 1, Left)
    }
  end,
  { Item, NewItems };

extract(Position, [ Item | Left ], SizeLeft, Right, SizeRight ) ->
  extract(Position - 1, Left, SizeLeft - 1, [ Item | Right ], SizeRight + 1).

simplify(Left, Right) ->
  LS = size(Left),
  RS = size(Right),
  case { LS, RS } of
    { 0, 0 } -> [];
    { 0, _ } -> Right#random_list.items;
    { _ , 0 } -> Left#random_list.items;
    _ -> #rl_pair{ left = Left, right = Right }
  end.

%% -----------------------------------------------------
%% TESTS
%% -----------------------------------------------------

-ifdef(TEST).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

t0_test() ->
  N = 10000,
  Count = lists:seq(1,N),
  List = lists:map(fun(_Index) -> rand:uniform(N) end, Count),
  R = random_list:new(List),
  { U, _ } = lists:foldl(fun(_Index, { Acc, R0 }) ->
    { ok, El, R1 } = random_list:pop(R0),
    { [ El | Acc ], R1 }
  end, { [], R }, Count),
  ?assertEqual([], List -- U),
  ?assertEqual([], U -- List),
  ok.

t1_test() ->
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

t2_test() ->
  List = [ 1,2,3 ],
  R = random_list:new(List),
  Res0 = random_list:fold(fun(Item, Acc) -> [ Item | Acc ] end, [], R),
  ?assertEqual([], Res0 -- List),
  Res1 = random_list:to_list(random_list:map(fun(Item) -> Item end, R)),
  ?assertEqual([], Res1 -- List),
  ok.

t3_test() ->
  List = [ 1,2,3 ],
  R = random_list:new(List),
  { ok, Res } = random_list:get(R),
  ?assertEqual(true, lists:member(Res, List)),
  ok.

t4_test() ->
  Iterations = 100,
  List = [ 1,2,3,4,5,6,7,8,9 ],
  R = random_list:new(List),
  R0 = lists:foldl(fun(_I, Acc) ->
    { ok, _, NewAcc} = random_list:pop_push(Acc),
    NewAcc
  end, R, lists:seq(1, Iterations)),
  ?assertEqual(lists:sort(List), lists:sort(random_list:to_list(R0))),
  ?assertEqual(lists:sort(random_list:to_list(R)), lists:sort(random_list:to_list(R0))),
  ok.

filter_test() ->
  List = [ 1,2,3,4,5,6,7,8,9 ],
  Rule = fun(A) -> A rem 2 =:= 0 end,
  L0 = lists:filter(Rule, List),
  { ok, _, R } = random_list:pop_push(random_list:new(List)),
  L1 = random_list:to_list(random_list:filter(Rule, R)),
  ?assertEqual([], L0 -- L1),
  ?assertEqual([], L1 -- L0),
  ok.

empty_list_test() ->
  EmptyList = random_list:new(),
  ?assertEqual({ error, list_is_empty }, random_list:pop(EmptyList)),
  ?assertEqual({ error, list_is_empty }, random_list:get(EmptyList)),
  ok.

remove_test() ->
  R = random_list:new([a]),
  { changed, NewR } = random_list:remove(R, a),
  ?assert(is_empty(NewR)),
  { not_found, R} = remove(R, b),
  ok.

partition_test() ->
  L = [1,2,3,4,5,6,7,8,9,0],
  R = new(L),
  F = fun(X) -> X rem 2 =:= 0 end,
  { R1, R0 } = partition(F, R),
  { L1, L0 } = lists:partition(F, L),
  ?assertEqual(lists:sort(L1), lists:sort(to_list(R1))),
  ?assertEqual(lists:sort(L0), lists:sort(to_list(R0))),
  ok.

-endif.