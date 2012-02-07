bubblesort(List, Answer) :- bubblesort_pass(List, Swapped, IntAnswer), Swapped == yes, bubblesort(IntAnswer, Answer).
bubblesort(List, Answer) :- bubblesort_pass(List, Swapped, Answer), Swapped == no.
bubblesort_pass([Head|[]], no, [Head]).
bubblesort_pass([First|[Second|Tail]], Swapped, [First|SubAnswer]) :- First =< Second, bubblesort_pass([Second|Tail], Swapped, SubAnswer).
bubblesort_pass([First|[Second|Tail]], yes, [Second|SubAnswer]) :- First > Second, bubblesort_pass([First|Tail], _, SubAnswer).
