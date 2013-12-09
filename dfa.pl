start(q1).

final(q1).

delta(q1,b,q1).
delta(q2,b,q2).
delta(q1,a,q2).
delta(q2,a,q1).

accept(S) :- start(Q),run(Q,S).

run(Q,[A|As]) :- delta(Q,A,NewQ), run(NewQ,As).
run(Q,[]) :- final(Q).