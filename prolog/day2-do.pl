betzel([Head|[]], [Head]).
betzel([],[]).

betzel([Head|Tail], A) :- betzel(Tail, B), append(B, [Head], A).
