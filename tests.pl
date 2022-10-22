:- consult('Dfa.pl'). %consult your solution here

example(a11, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1)], 1, [2,1])).
example(a12, dfa([fp(x,a,y),fp(x,b,x),fp(y,a,x),fp(y,b,x)], x, [x,y])).
example(a2, dfa([fp(1,a,2),fp(2,b,1),fp(1,b,3),
                 fp(2,a,3),fp(3,b,3),fp(3,a,3)], 1, [1])).
example(a3, dfa([fp(0,a,1),fp(1,a,0)], 0, [0])).
example(a4, dfa([fp(x,a,y),fp(y,a,z),fp(z,a,x)], x, [x])).
example(a5, dfa([fp(x,a,y),fp(y,a,z),fp(z,a,zz),fp(zz,a,x)], x, [x])).
example(a51, dfa([fp(x,1,y),fp(y,1,z),fp(z,1,zz),fp(zz,1,x),
                 fp(x,0,bad),fp(y,0,bad),fp(z,0,bad),fp(zz,0,bad),
                 fp(bad,0,bad), fp(bad,1,bad)], x, [x])). %same as a5 but with one more letter + limbo state
example(a6, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1)], 1, [])).
example(a7, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1),
                 fp(3,b,3),fp(3,a,3)], 1, [3])).
example(a71, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1),
                  fp(3,b,3),fp(3,a,3),fp(4,a,4),fp(4,b,4)], 1, [3])).
example(a72, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1),
                  fp(3,b,3),fp(3,a,3),fp(4,a,4),fp(4,b,4)], 3, [3])).
example(a8, dfa([fp(1,a,2), fp(1,b,5), fp(1,c,5), fp(2,a,5), fp(2,b,3), 
                 fp(2,c,5), fp(3,a,5), fp(3,b,5), fp(3,c,4), fp(4,a,5),
                 fp(4,b,5), fp(4,c,5), fp(5,b,5), fp(5,c,5), fp(5,a,5)], 1, [4])).
example(a81, dfa([fp(1,a,2), fp(1,b,5), fp(1,c,5), fp(2,a,5), fp(2,b,3), 
                 fp(2,c,5), fp(3,a,5), fp(3,b,5), fp(3,c,4), fp(4,a,5),
                 fp(4,b,5), fp(4,c,5), fp(5,b,5), fp(5,c,5), fp(5,a,5)], 1, [1,2,3,4])).
example(a9, dfa([fp(1,a,2), fp(1,b,4), fp(1,c,4), fp(2,a,4), fp(2,b,3), 
                 fp(2,c,4), fp(3,a,4), fp(3,b,4), fp(3,c,1), fp(4,a,4),
                 fp(4,b,4), fp(4,c,4)], 1, [1])).
example(c1, dfa([fp(1,a,1), fp(1,b,1), fp(2,a,3), fp(2,b,3), fp(3,a,4), 
                 fp(3,b,4), fp(4,a,5), fp(4,b,5), fp(5,b,2), fp(5,a,2)], 1, [2,3,4,5])).
example(c2, dfa([fp(1,a,1), fp(1,b,1), fp(1,c,1), fp(2,a,3), fp(2,b,3), 
                 fp(2,c,3), fp(3,a,4), fp(3,b,4), fp(3,c,4), fp(4,a,5),
                 fp(4,b,5), fp(4,c,5), fp(5,b,2), fp(5,c,2), fp(5,a,2)], 1, [2,3,4,5])).
example(g1, dfa([fp(q0,1,q0), fp(q0,0,q1), fp(q1,1,q2), fp(q1,0,q1), fp(q2,0,q1), 
                 fp(q2,1,q0)], q0, [q2])). % language of words ending in 01
example(g2, dfa([fp(q0,0,q0), fp(q0,1,q1), fp(q1,1,q0), fp(q1,0,q1)], 
                 q0, [q0])). %language of even number of 1
example(g3, dfa([fp(q0,1,q0), fp(q0,0,q1), fp(q1,0,q2), fp(q2,1,q2), 
                 fp(q1,1,limbo), fp(q2,0,limbo),
                 fp(limbo,0,limbo), fp(limbo,1,limbo)], q0, [q2])). %language of two zeros in the middle

examplebad(b1, dfa([fp(1,a,1),fp(1,a,1)], 1, [])).
examplebad(b2, dfa([fp(1,a,1),fp(1,a,2)], 1, [])).
examplebad(b3, dfa([fp(1,a,2)], 1, [])).
examplebad(b4, dfa([fp(1,a,1)], 2, [])).
examplebad(b5, dfa([fp(1,a,1)], 1, [1,2])).
examplebad(b6, dfa([], [], [])).

emptygood(a6).
emptygood(a7).
emptygood(a71).
emptygood(c1).
emptygood(c2).

emptybad(a11).
emptybad(a12).
emptybad(a2).
emptybad(a3).
emptybad(a4).
emptybad(a5).
emptybad(a51).
emptybad(a72).
emptybad(a8).
emptybad(a81).
emptybad(a9).
emptybad(g1).
emptybad(g2).
emptybad(g3).

equalgood(a11, a12).
equalgood(a4, a4).
equalgood(a7, a71).
equalgood(a72, a72).
equalgood(a72, a12).
equalgood(a7, c1).

equalbad(a3, a4).
equalbad(a3, a5).
equalbad(a8, a9).
equalbad(a8, a81).
equalbad(a7, c2). % Alphabets not equal
equalbad(g1, g2).

subsetgood(a2, a11).
subsetgood(a5, a3).
subsetgood(a7, a71).
subsetgood(a71, a7).
subsetgood(a8, a9).
subsetgood(a8, a81).
subsetgood(c1, a2).
subsetgood(a51, g2).

subsetbad(a4, a3).
subsetbad(a3, a4).
subsetbad(a81, a8).
subsetbad(a2, c1).
subsetbad(c2, a3). % Alphabets not equal
subsetbad(g1, g2).
subsetbad(g2, g1).
subsetbad(g2, a51).

long(a8).
long(a81).
long(a9).
long(a51).

acceptWord(a2, []).
acceptWord(a2, [a, b]).
acceptWord(a2, [a, b, a, b]).

acceptWordBad(a2, [a]).
acceptWordBad(a2, [1]).

findWord(a11, [X, X, X, X, X, X]). % aaaaaa + bbbbbb
findWord(a11, [X, Y, Z]). % Every possible combination
findWord(a2, [X, Y]). % ab
findWord(a2, [U, V, W, X, Y, Z]). % ababab
findWord(a5, [A, B, C, D, E, F, G, H]). % aaaaaaaa
findWord(a5, [A, A, A, A, A, A, A, A]). % aaaaaaaa
findWord(a72, [X, Y]). % Every possible combination
findWord(a81, [A]). % a
findWord(a81, [A, B]). % ab
findWord(a81, [A, B, C]). % abc
findWord(a9, [A, B, C]). % abc
%findWord(a9, [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]). % abcabcabcabcabc %mój komputer się tu trochę napocił
findWord(a9, [A, B, C, A, B, C, A, B, C, A, B, C, A, B, C]). % abcabcabcabcabc %a tu już nie
findWord(g1, [A, B, C, D]). % 4 words, all end in 01
findWord(g2, [A, B, C, D]). % all have even no of 1
findWord(g3, [A, B, C, D, E, F]). % two zeros next to each other, rest are 1.

findWordBad(a2, [X]).
findWordBad(a2, [X, Y, Z]).
findWordBad(a7, [X]).
findWordBad(a71, [X]).
findWordBad(a9, [A, B, C, D, E, F, G, H, I, J, K, L, M, N]).
findWordBad(g3, [X]).

% Check that representation is good without prolog being a bitch about how long it is  
testLong(X) :-
    long(X),
    example(X, Y),
    correct(Y, rep(Alp, Sne, Sta, End)), % change rep to your representation
    write(Alp),
    write('\n'),
    write(Sne),
    write('\n'),
    write(Sta),
    write('\n'),
    write(End).

testCorrect(Z) :-
    example(X, Y),
    write("Now testing: "),
    write(X),
    write(" should be correct"),
    correct(Y, Z).

testCorrectBad() :-
    examplebad(X, Y),
    write("Now testing: "),
    write(X),
    write(" should not be correct"),
    write('\n'),
    \+ correct(Y, _).

testAcceptWord() :-
    acceptWord(X, W),
    example(X, Y),
    write("Now testing: "),
    write(X),
    write(" should accept word "),
    write(W),
    write('\n'),
    accept(Y, W).

testAcceptWordBad() :-
    acceptWordBad(X, W),
    example(X, Y),
    write("Now testing: "),
    write(X),
    write(" should not accept word "),
    write(W),
    write('\n'),
    \+ accept(Y, W).

testFindWord(W) :-
    findWord(X, W),
    example(X, Y),
    write("Now testing: "),
    write(X),
    write(" should find some words matching "),
    write(W),
    write('\n'),
    accept(Y, W).

testFindWordBad(W) :-
    findWordBad(X, W),
    example(X, Y),
    write("Now testing: "),
    write(X),
    write(" should find none words matching "),
    write(W),
    write('\n'),
    \+ accept(Y, W).

testEmpty() :-
    emptygood(X),
    example(X, Y),
    write("Now testing: "),
    write(X),
    write(" should be empty"),
    empty(Y).

testEmptyBad() :-
    emptybad(X),
    example(X, Y),
    write("Now testing: "),
    write(X),
    write(" should not be empty"),
    write('\n'),
    \+ empty(Y).

testEqual() :-
    equalgood(X, Y),
    write("Now testing: "),
    write(X),
    write(" == "),
    write(Y),
    write('\n'),
    example(X, A), example(Y, B), equal(A, B).

testSubset() :-
    subsetgood(X, Y),
    write("Now testing: "),
    write(X),
    write(" c "),
    write(Y),
    write('\n'),
    example(X, A), example(Y, B), subsetEq(A, B).

testEqualBad() :-
    equalbad(X, Y),
    write("Now testing: "),
    write(X),
    write(" != "),
    write(Y),
    write('\n'),
    example(X, A), example(Y, B), \+ equal(A, B).

testSubsetBad() :-
    subsetbad(X, Y),
    write("Now testing: "),
    write(X),
    write(" !c "),
    write(Y),
    write('\n'),
    example(X, A), example(Y, B), \+ subsetEq(A, B).
