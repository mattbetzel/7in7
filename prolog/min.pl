min([A|[]], A).

%min([Head|Tail], C) :- =<(C, Head), min(Tail, C).

min([Head|Tail], C) :- min(Tail, C), =<(C, Head).
min([Head|Tail], Head) :- min(Tail, C), =<(Head, C).
