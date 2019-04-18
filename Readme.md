# Random access list

This library is implementing list to access element randomly.
It can collect many thousands elements and it have good performance to extract element randomly.

# Usage

```
List0 = random_list:new([ 1,2,3,4,5 ]), % initialise list by elements
List1 = random_list:push(List0, 6), % push new element into list
{ ok, El, List2 } = random_list:pop(List1), % get and delete random element from list
```

# LICENSE

MIT