/*
 * My first implementation using an accumulator of current smallest
 */
smallest([Head|Tail], Answer) :- smallest([Head|Tail], Head, Answer).
smallest([], Answer, Answer).
smallest([Head|Tail], CurrentSmallest, Answer) :- Head =< CurrentSmallest, smallest(Tail, Head, Answer).
smallest([Head|Tail], CurrentSmallest, Answer) :- Head > CurrentSmallest, smallest(Tail, CurrentSmallest, Answer).

/*
 * A second implementation using min logic
 */
min(First, Second, First) :- First =< Second.
min(First, Second, Second) :- Second < First.
small2([Head|[]], Head).
small2([Head|Tail], Answer) :-  small2(Tail, TailAnswer), min(Head, TailAnswer, Answer).
