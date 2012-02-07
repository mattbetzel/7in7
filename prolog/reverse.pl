reverse([], []).
reverse([Head|[]], [Head]).
%reverse([Head1|[Head2|[]]], [Head2, Head1]).
%reverse([Head1|[Head2|[Head3|[]]]], [Head3, Head2, Head1]).
%
%reverse (x:[]) = [x]
%reverse (x:xs) = reverse xs ++ [x]

%reverse([Head1|List1], List2) :- append(reverse(List1, List3), [Head1], List2).

reverse([Head1|List1], List2) :- reverse(List1, List3), append(List3, [Head1], List2).
