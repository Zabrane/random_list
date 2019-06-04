# Random access list

This library is implementing list (set) to access element randomly.
It can collect many thousands elements and it have good performance to extract element randomly.

# Modules

* __random_list__ is implementation of random list to exctract elements easily without weight for elements. All elements are equiprobable.
* __random_list_weight__ is implementation of random list to extract elements easily with weight of element.   

# Usage

## random_list

```
List0 = random_list:new([ 1,2,3,4,5 ]), % initialise list by elements
List1 = random_list:push(List0, 6), % push new element into list
{ ok, El, List2 } = random_list:pop(List1), % get and delete random element from list
```

## random_list_weight

```
List = random_list_weight:new([{1, a}, {9, b}]), %% { Weight, Element }
{ ok, Weight, El, NewList } = random_list_weight:pop(List)
```

There are many other functions for all these modules so just check code and see what it can do.

# LICENSE

MIT