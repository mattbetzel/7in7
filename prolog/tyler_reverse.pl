rev([], []).
rev([Head|[]], [Head]).
rev([Head|Tail], A) :- append(B, [Head], A), rev(Tail, B).

rev2(StartList, Solution) :- rev2(StartList, [], Solution).
rev2([], Accum, Accum).
rev2([Head|Tail], Accum, Solution) :- rev2(Tail, [Head|Accum], Solution).
