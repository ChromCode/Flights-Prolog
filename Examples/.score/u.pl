
test(Abbr) :-
    airport(Abbr, _, L, _),
    L = degmin(X, _),
    write(X), nl.

