
-module(random_list_weight).

-compile({no_auto_import, [ size/1, get/1 ]}).

%% API
-export([
  new/0, new/1,
  size/1, is_empty/1, propability/2,
  push/3, merge/2,
  pop/1, get/1, pop_push/1,
  balance/1,
  to_list/1,
  fold/3, foreach/2,
  map/2, filter/2
]).

-define(MINIMAL_LIST, 5). %% Not separated minimal list

-record(rl_pair, {
  left,
  right
}).

-record(item, {
  weight :: pos_integer(),
  element :: term()
}).

-record(random_list_weight, {
  size = 0 :: pos_integer(),
  sum_weight = 0 :: pos_integer(),
  items = [] :: [#item{}] | #rl_pair{},
  is_balanced = false :: boolean()
}).

-type random_list() :: #random_list_weight{}.

-export_type([
  random_list/0
]).

new() -> #random_list_weight{}.

new(List) ->
  { Items, SumWeight } = lists:mapfoldl(fun
    ({Weight, Element}, Sum) ->
      { #item{ weight = Weight, element = Element }, Sum + Weight}
  end, 0, List),

  balance(#random_list_weight{
    sum_weight = SumWeight,
    size = length(Items),
    items = Items
  }).

size(List) ->
  List#random_list_weight.size.

sum_weight(List) ->
  List#random_list_weight.sum_weight.

is_empty(List) ->
  size(List) =:= 0.

balance(R = #random_list_weight{ is_balanced = true}) -> R;

balance(R = #random_list_weight{ items = List }) when is_list(List) ->
  R#random_list_weight{
    items = lists:reverse(lists:keysort(#item.weight, List)),
    is_balanced = true
  };

balance(R = #random_list_weight{ items = #rl_pair{left = Left, right = Right}}) ->

  R#random_list_weight{
    items = simplify(#rl_pair{
      left = balance(Left),
      right = balance(Right)
    }),
    is_balanced = true
  }.

propability(List, Weight) ->
  Weight/sum_weight(List).

push(List = #random_list_weight{ items = L }, Weight, Item) when is_list(L) ->
  #random_list_weight{
    sum_weight = sum_weight(List) + Weight,
    size = size(List) + 1,
    items = [ #item{ weight = Weight, element = Item } | List#random_list_weight.items ],
    is_balanced = false
  };

push(List = #random_list_weight{ items = #rl_pair{ left = Left, right = Right } = Items}, Weight, Item) ->
  NewItems = case sum_weight(Left) + Weight =< sum_weight(Right) of
    true ->
      Items#rl_pair{
        left = push(Left, Weight, Item)
      };
    false ->
      Items#rl_pair{
        right = push(Right, Weight, Item)
      }
    end,
  List#random_list_weight{
    sum_weight = sum_weight(List) + Weight,
    size = size(List) + 1,
    items = NewItems,
    is_balanced = false
  }.



pop(#random_list_weight{ size = 0 }) -> {error, list_is_empty};
pop(#random_list_weight{ size = 1, items = [#item{ weight = Weight, element = Element} ]}) ->
  { ok, Weight, Element, new() };

pop(List) ->
  Weight = rand:uniform(sum_weight(List)),
  pop(Weight, List).

pop_push(List) ->
  case pop(List) of
    { ok, Weight, Item, NewList } -> { ok, Weight, Item, push(NewList, Weight, Item) };
    Any -> Any
  end.

get(#random_list_weight{ size = 0}) -> { error, list_is_empty };
get(List) ->
  Weight = rand:uniform(sum_weight(List)),
  get(Weight, List).

to_list(#random_list_weight{ size = 0 }) -> [];
to_list(#random_list_weight{ items = List }) when is_list(List) -> [ { Weight, El } || #item{ weight = Weight, element = El } <- List ];
to_list(#random_list_weight{ items = #rl_pair{ left = Left, right = Right } }) ->
  SizeLeft = size(Left),
  SizeRight = size(Right),
  if
    SizeLeft =< SizeRight -> to_list(Left) ++ to_list(Right);
    true -> to_list(Right) ++ to_list(Left)
  end.

merge(R1 = #random_list_weight{ items = List1, size = Size1 }, R2 = #random_list_weight{ items = List2, size = _Size2 }) when is_list(List1), is_list(List2), Size1 < ?MINIMAL_LIST ->
  #random_list_weight{
    size = size(R1) + size(R2),
    sum_weight = sum_weight(R1) + sum_weight(R2),
    items = List1 ++ List2,
    is_balanced = false
  };

merge(R1 = #random_list_weight{ items = List1, size = Size1 }, R2 = #random_list_weight{ items = List2, size = Size2 }) when is_list(List1), is_list(List2), Size2 < ?MINIMAL_LIST, Size1 >= ?MINIMAL_LIST ->
  merge(R2, R1);

merge(R1 = #random_list_weight{ items = List1 }, R2 = #random_list_weight{ items = List2 }) when is_list(List1), is_list(List2) ->
  Items = case sum_weight(R1) > sum_weight(R2) of
    true ->
      #rl_pair{ left = R1, right = R2 };
    false ->
      #rl_pair{ left = R2, right = R1 }
  end,
  #random_list_weight{
    size = size(R1) + size(R2),
    sum_weight = sum_weight(R1) + sum_weight(R2),
    items = Items,
    is_balanced = false
  };

merge(R1 = #random_list_weight{ items = #rl_pair{ left = Left1, right = Right1 } }, R2 = #random_list_weight{ items = #rl_pair{ left = Left2, right = Right2 }}) ->
  #random_list_weight{
    size = size(R1) + size(R2),
    sum_weight = sum_weight(R1) + sum_weight(R2),
    items = #rl_pair{
      left = merge(Left1, Left2),
      right = merge(Right1, Right2)
    },
    is_balanced = false
  };

merge(R1 = #random_list_weight{ items = #rl_pair{ left = Left1, right = Right1 } }, R2 = #random_list_weight{ items = _List2 }) ->
  D = sum_weight(R2),
  NewItems = case sum_weight(Left1) + D =< sum_weight(Right1) of
    true ->
      #rl_pair{
        left = merge(Left1, R2),
        right = Right1
      };
    false ->
      #rl_pair{
        left = Left1,
        right = merge(Right1, R2)
      }
  end,
  #random_list_weight{
    size = size(R1) + size(R2),
    sum_weight = sum_weight(R1) + sum_weight(R2),
    items = NewItems,
    is_balanced = false
  };

merge(R1 = #random_list_weight{ items = List1 }, R2 = #random_list_weight{ items = #rl_pair{} }) when is_list(List1) ->
  merge(R2, R1).


%% High order functions

map(Fun, #random_list_weight{ items = List }) when is_list(List) ->
  NewItems = lists:map(fun(#item{ weight = Weight, element = El}) ->
    Fun({Weight, El})
  end, List),
  new(NewItems);

map(Fun, #random_list_weight{ items = #rl_pair{ left = Left, right = Right } }) ->
  new(map(Fun, Left) ++ map(Fun, Right)).

filter(Fun, #random_list_weight{ items = List }) when is_list(List) ->
  NewItems = lists:filter(fun(#item{ weight = Weight, element = El }) ->
    Fun({Weight, El})
  end, List),
  new(NewItems);

filter(Fun, #random_list_weight{ items = #rl_pair{ left = Left, right = Right } }) ->
  NewLeft = filter(Fun, Left),
  NewRight = filter(Fun, Right),
  new(NewLeft ++ NewRight).

fold(Fun, Acc, List) ->
  case is_empty(List) of
    true -> Acc;
    false ->
      { ok, Weight, Item, NewList} = pop(List),
      fold(Fun, Fun({Weight, Item}, Acc), NewList)
  end.

foreach(Fun, List) ->
  fold(fun(Item, _) ->
    Fun(Item),
    ok
  end, ok, List).

%% Internal

pop(_, #random_list_weight{ size = 0 }) -> { error, list_is_empty };
pop(Weight, #random_list_weight{ sum_weight = SumWeight}) when Weight > SumWeight -> { error, bad_weight };
pop(Weight, List) ->
  { ok, Item, NewR} = extract(Weight, List, new()),
  { ok, Item#item.weight, Item#item.element, NewR }.

get(WeightPos, #random_list_weight{ sum_weight = SumWeight }) when WeightPos > SumWeight -> { error, bad_weight };
get(WeightPos, R = #random_list_weight{ items = List }) when is_list(List) ->
  case List of
    [] -> { error, list_is_empty };
    [#item{ weight = Weight, element = _ } | Tail ] when WeightPos > Weight ->
      NewList = R#random_list_weight{
        size = size(R),
        sum_weight = sum_weight(R) - Weight,
        items = Tail
      },
      get(WeightPos - Weight, NewList);
    [#item{ weight = Weight, element = Element } | _ ] ->
      { ok, Weight, Element }
  end;

get(WeightPos, #random_list_weight{ items = #rl_pair{ left = Left, right = Right } }) ->
  Delta = sum_weight(Left) - WeightPos,
  case Delta >= 0 of
    true -> get(Delta, Left);
    false -> get(-Delta, Right)
  end.

extract(WeightPos, R = #random_list_weight{ items = [ Item = #item{ weight = Weight, element = El } | Tail ]}, Acc)  ->
  case WeightPos =< Weight of
    true ->
      NewR = #random_list_weight{
        size = size(R) - 1,
        sum_weight = sum_weight(R) - Weight,
        items = Tail
      },
      { ok, Item, merge(Acc, NewR)};
    false ->
      NewR = R#random_list_weight{
        size = size(R) - 1,
        sum_weight = sum_weight(R) - Weight,
        items = Tail
      },
      extract(WeightPos - Weight, NewR, push(Acc, Weight, El))
  end;

extract(WeightPos, #random_list_weight{ items = #rl_pair{ left = Left, right = Right}}, Acc) ->
  case WeightPos =< sum_weight(Left) of
    true ->
      extract(WeightPos, Left, merge(Right, Acc));
    false ->
      extract(WeightPos - sum_weight(Left), Right, merge(Left, Acc))
  end.

%% INTERNAL

simplify(#rl_pair{ left = Left, right = Right}) ->
  LS = size(Left),
  RS = size(Right),
  case { LS, RS } of
    { 0, 0 } -> [];
    { 0, _ } -> Right#random_list_weight.items;
    { _ , 0 } -> Left#random_list_weight.items;
    _ -> #rl_pair{ left = Left, right = Right }
  end.

%% -----------------------------------------------------
%% TESTS
%% -----------------------------------------------------

-ifdef(TEST).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

t0_test() ->
  List =  lists:seq(1, 100),
  P = [ {1, X} || X <- List ],
  R = random_list_weight:new(P),
  { ok, 1, A, R0 } = random_list_weight:pop(R),
  ?assertEqual(true, lists:member(A, List)),
  { ok, 1, B, R1 } = random_list_weight:pop(R0),
  ?assertEqual(true, lists:member(B, List -- [ A ]) ),
  { ok, 1, C, R2 } = random_list_weight:pop(R1),
  ?assertEqual(true, lists:member(C, List -- [ A, B ]) ),
  ?assertEqual(97, random_list_weight:size(R2)),
  ok.

t1_test() ->
  R = random_list_weight:new([{1, 1}, {9, 2}]),
  { A0, B0 } = lists:foldl(fun(_, {A, B}) ->
    case random_list_weight:get(R) of
      { ok, _, 1 } -> {A+1, B};
      { ok, _, 2 } -> {A, B + 1 }
    end
  end, {0,0}, lists:seq(1, 10000)),
  P = A0/(A0+B0),
  ?assert( P >= 0.07 ),
  ?assert( P < 0.12 ),
  ok.

t2_test() ->
  EmptyList = random_list_weight:new(),
  ?assertEqual({ error, list_is_empty }, random_list_weight:pop(EmptyList)),
  ?assertEqual({ error, list_is_empty }, random_list_weight:get(EmptyList)),
  ok.

-endif.
