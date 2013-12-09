parent(jan,derek).
parent(scott,derek).
parent(jan,erica).
parent(scott,erica).

parent(howard,scott).
parent(barbara,scott).
parent(grumpy,jan).
parent(grandmastrolin,jan).

parent(karen,kelsey).
parent(karen,reese).
parent(howard,karen).
parent(barbara,karen).

child(X,Y) :- parent(Y,X).
sibling(X,Y) :- parent(Z,X),parent(Z,Y),X\==Y.
grandparent(X,Y) :- parent(Z,Y),parent(X,Z).
grandchild(X,Y) :- grandparent(Y,X).
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(Z,Y),ancestor(X,Z).
descendant(X,Y) :- ancestor(Y,X).
pibling(X,Y) :- parent(Z,Y),sibling(X,Z). %% aunt/uncle
nibling(X,Y) :- pibling(Y,X). %% niece/nephew
firstcousin(X,Y) :- grandparent(Z,X),grandparent(Z,Y),X\==Y, \+(sibling(X,Y)).
firstcousinonceremoved(X,Y) :- parent(Z,Y),firstcousin(X,Z). %% one generation down cousins
