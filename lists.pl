appendList([],Ys,Ys).
appendList([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).

lastList([A],A).
lastList([X|Xs],Y) :- last(Xs,Y).

deleteList(X,[X|Xs],Xs).
deleteList(X,[Y|Xs],[Y|Ys]) :- deleteList(X,Xs,Ys).

permList([],[]).
permList(Xs,[Y|Ys]) :- deleteList(Y,Xs,Zs),permList(Zs,Ys).

orderedList([]).
orderedList([_|[]]).
orderedList([X|[Y|Ys]]) :- X=<Y,orderedList([Y|Ys]).

lsortList(Xs,Ys) :- permList(Xs,Ys),orderedList(Ys).