-module(random_list).

-compile({no_auto_import, [ size/1 ]}).

%% API
-export([
  new/0, new/1, new/2,
  size/1, is_empty/1,
  push/2,
  pop/1,
  to_list/1
]).

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

pop(#random_list{ size = 1, items = [Item]}) ->
  { ok, Item, new() };

pop(List) ->
  Position = rand:uniform(size(List)),
  pop_by_position(Position, List).

to_list(List = #random_list{ size = 0 }) -> [];
to_list(List = #random_list{ items = List }) when is_list(List) -> List;
to_list(List = #random_list{ items = #rl_pair{ left = Left, right = Right } }) ->
  SizeLeft = size(Left),
  SizeRight = size(Right),
  if
    SizeLeft =< SizeRight -> to_list(Left) ++ to_list(Right);
    true -> to_list(Right) ++ to_list(Left)
  end.

%% Internal

pop_by_position(_, #random_list{ size = 0 }) -> { error, list_is_empty };
pop_by_position(Position, #random_list{ size = Size }) when Position > Size -> { error, bad_position };
pop_by_position(Position, List) ->
  #random_list{
    size = Size,
    items = Items
  } = List,
  { Item, NewSize, NewItems } = extract(Position, Size, Items),
  { ok, Item, #random_list{
    size = NewSize,
    items = NewItems
  }}.

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