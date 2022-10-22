%Konrad Korczyński
%Internal Automata Representation:
%Bst Tree designated with bst/3 predicate
%Node of the tree is designated as node/3 - node(Id, IsAcc, Funcs) where
%Id - State identifier
%IsAcc - boolean value that designates if the state is accepting
%Funcs - list of transition functions for that state

%%=============================== BST TREE SECTION -===========================%
%'greater' predicate for building bst trees
gt(node(X, _, _), node(Y, _, _)) :- X @> Y.

%isAcc(+X, +Accs, -IsAcc)
%Given (X - node identifier) and (Accs - accepting states list)
%Returns (IsAcc - whether X is accepting state)
isAcc(X, Accs, IsAcc) :- 
    (member(X, Accs) -> IsAcc=true ; IsAcc=false).
 
%---------------------------- CREATE NODE -------------------------------------%
%createNode(+OldTree, +NewNode, -NewTree)
%Given (OldTree - original bst tree) and (NewNode - node() to insert)
%Returns (NewTree - OldTree with NewNode inserted)

%empty OldTree case with no edges; insert node with empty list
createNode(nil, node(X, IsAcc, nil), 
            bst(node(X, IsAcc, []), nil ,nil)).
%emptyOldTree with edge; insert node with 1-element list [edge]
createNode(nil, node(X, IsAcc, edge(Letter, To)), 
            bst(node(X, IsAcc, [edge(Letter, To)]), nil ,nil)).

%CurrentNode == NewNode case with no edges; leave edge list unchanged
createNode(bst(node(X, IsAcc, Funcs), Left, Right),
            node(X, IsAcc, nil), 
            bst(node(X, IsAcc, Funcs), Left, Right)):-!.
%CurrentNode == NewNode case with edge; add new edge to the list
createNode(bst(node(X, IsAcc, Funcs), Left, Right),
            node(X, IsAcc, edge(Letter, To)), 
            bst(node(X, IsAcc, [edge(Letter, To) | Funcs]), Left, Right)):-!.

%Root > X; go to the left subtree
createNode(bst(Root, Left, Right), X, bst(Root, Left1, Right)):-
  gt(Root, X), !, 
  createNode(Left, X, Left1).

%X > Root; go to the right subtree
createNode(bst(Root, Left, Right), X, bst(Root, Left, Right1)):-
  gt(X, Root), !, 
  createNode(Right, X, Right1).

%--------------------------------- MAKE TREE ----------------------------------%
%makeTree(+Fs, +Accs, -AutomataTree)/3
%Given (Fs - list of fp()) and (Accs - list of accepting states)
%Returns (AutomataTree - automata converted to a bst tree 
%    with nodes = node(Identifier, IsAccepting, [edge(Letter, To) | _]))

%base case; empty list means empty tree
makeTree([], _, nil).

%invoking makeTree/4 with empty OldTree
makeTree(Fs, Accs, AutomataTree) :-
    makeTree(Fs, Accs, nil, AutomataTree).

%makeTree(+Fs, +Accs, +OldTree, -AutomataTree)
%same as makeTree/3, but takes (OldTree - accumulator tree)

%base case
makeTree([], _, Tree, Tree).
makeTree([fp(From, X, To) | Fs], Accs, OldTree, AutomataTree):-
    isAcc(From, Accs, IsAcc1), !, %tell if first state is accepting
    isAcc(To, Accs, IsAcc2), !,   %tell if second state is accepting
    %insert first node    
    createNode(OldTree, node(From, IsAcc1, edge(X, To)), NewTree1), !,
    %insert second node with no edges as we only know that it exists,
    %not its transitions
    createNode(NewTree1, node(To, IsAcc2, nil), NewTree2), !,
    %recursively go through Fs with new accumulator
    makeTree(Fs, Accs, NewTree2, AutomataTree).
           
%--------------------------------- FIND TREE ----------------------------------%
%findTree(+Tree, +ToFind, -FoundNode)
%Given (Tree - tree to search) and (ToFind - identifier of state to find)
%Returns (FoundNode - node() associated with ToFind)

%found
findTree(bst(node(X, IsAcc, Funcs), _, _), X, node(X, IsAcc, Funcs)):-!.
%go left
findTree(bst(node(Y, _, _), Left, _), X, Result) :-
    Y @> X, !, findTree(Left, X, Result).
%go right
findTree(bst(node(Y, _, _), _, Right), X, Result) :-
    X @> Y, !, findTree(Right, X, Result).

%============================== CORRECT SECTION ===============================%
%------------------------------- UNIQUE ---------------------------------------%
%unique(+X)
%Given (X - any list)
%Succseeds iff every element in the list is unique
unique(X):- 
    sort(X, Sorted),                %sorting discards duplicates
    length(X, OriginalLength),      
    length(Sorted, SortedLength),
    OriginalLength == SortedLength. %if anything was discarded then fail

%------------------------------- CONSISTENT -----------------------------------%
%consistent(+AutomataTree, +Start, +Accs)
%Given (AutomataTree - automata represented as bst tree)
%      (Start - Starting state)
%      (Accs - List of accepting states)
%Succseeds iff (Start in AutomataTree) and 
%              (ForEvery Acc in Accs -> Acc in AutomataTree)
consistent(AutomataTree, Start, Accs) :-
    findTree(AutomataTree, Start, _), !,    %check if start in AutomataTree
    consistent(AutomataTree, Accs).         %check accs

%consistent(+AutomataTree, +Accs)
%same as consistent/3, but only checks for Accs
%empty Accs always succseeds
consistent(_, []).

%recursively go through all elements in the list
consistent(AutomataTree, [Acc | Accs]) :- 
    findTree(AutomataTree, Acc, _), !,     %check if acc in AutomataTree
    consistent(AutomataTree, Accs).        %check next

%------------------------------- DETERMINISTIC --------------------------------%
%deterministic(+AutomataTree, +Alphabet)
%Given (AutomataTree - automata represented as bst tree) and 
%      (Alphabet - AutomataTree's automata alphabet)
%Succseeds iff automata is deterministic 
deterministic(bst(Root, Left, Right), Alphabet) :-
    deterministic(Root, Alphabet), !,   %check root
    deterministic(Left, Alphabet), !,   %go left
    deterministic(Right, Alphabet).     %go right 

%empty automata always succseeds
deterministic(nil, _).
%actually checking state for determinism
deterministic(node(_, _, Funcs), Alphabet) :-
    %get all transitions' letters
    findall(X, member(edge(X, _), Funcs), NodeAlphabet),               
    sort(NodeAlphabet, Alphabet),   %if fails then automata is incomplete
    length(NodeAlphabet, L1),
    length(Alphabet, L2),
    L1 == L2.                       %if fails then automata is non-deterministic

%------------------------------- GENERATE ALPHABET ----------------------------%
%generateAlphabet(+Funcs, -Alphabet)
%Given (Funcs - transition functions)
%Returns (Alphabet - alphabet of so defined automata)
generateAlphabet(Funcs, Alphabet) :- 
    findall(X, member(fp(_, X, _), Funcs), Alphabet1), %get all letters
    sort(Alphabet1, Alphabet).                         %discard duplicates

%+++++++++++++++++++++++++++ CORRECT ++++++++++++++++++++++++++++++++++++++++++%
correct(dfa(Funcs, Start, Accs), automata(Start, Alphabet, AutomataTree)) :-
    use_module(library(lists)), !, 
    is_list(Funcs), !, is_list(Accs), !,    %both fields must be lists
    ground(dfa(Funcs, Start, Accs)), !,     %dfa must contain only terms
    unique(Accs), !,                        %all accepting states must be unique
    unique(Funcs), !,                       %all definitions must be unique
    generateAlphabet(Funcs, Alphabet), !,   %return alphabet of automata
    makeTree(Funcs, Accs, AutomataTree), !, %create bst tree structure 
                                            %bst(node(State, IsAcc, [edge()]), 
                                            %                       Left, Right)
    consistent(AutomataTree, Start, Accs), !,%starting state and all 
                                             %accepting states must be present 
                                             %in automata
    deterministic(AutomataTree, Alphabet), !.%automata must be a dfa
%======================== ACCEPT SECTION ======================================%
%------------------------- FIND PATH ------------------------------------------%
%findPath(+Word, +Current, +AutomataTree)
%Given (Word - list of letters (can be non-terms)) and 
%      (Current - starting node)
%      (AutomataTree - bst tree structured automata)
%succseeds iff AutomataTree accepts Word starting from Current

%base case; either empty word or traversal has concluded
findPath([], Current, AutomamataTree) :-
    findTree(AutomamataTree, Current, node(Current, IsAcc, _)), !,
    IsAcc. %check if we are at accepting state

%go recursively through every letter in Word
findPath([Letter | Rest], Current, AutomamataTree) :-
    findTree(AutomamataTree, Current, node(Current, _, Funcs)), !,
    %get Letter lettered transition 
    findall(To, member(edge(Letter, To), Funcs), [GoTo]), !,
    %Choose that transition and repeat
    findPath(Rest, GoTo, AutomamataTree).

%+++++++++++++++++++++++++++++ ACCEPT +++++++++++++++++++++++++++++++++++++++++%
accept(A, L) :-
    use_module(library(lists)), !,
    is_list(L), !,      %No var implementation
    correct(A, automata(Start, _, AutomamataTree)), !, %A must be correct dfa
    findPath(L, Start, AutomamataTree). %A accepts L iff there is a path
%========================== EMPTY SECTION =====================================%
%--------------------------- SEEK ACCEPT --------------------------------------%
%seekAccept(+Automata)
%Given (Automata - internal automata structure)
%Succseeds iff its possible to get to accepting state from starting state
seekAccept(automata(Start, _, AutomataTree)) :-
    seekAccept([Start], AutomataTree, nil). %invoke seekAccept/3

%seekAccept(+RestToVisit, +AutomataTree, +Visited)
%simple dfs algorithm

%skip visited
seekAccept([X | ToVisit], AutomataTree, Visited) :-
    findTree(Visited, X, _), !,                 %X was already visited
    seekAccept(ToVisit, AutomataTree, Visited). %go for next state to visit

%check if accepting
seekAccept([X | _], AutomataTree, Visited) :-
    \+(findTree(X, Visited, _)),                %X was not visited
    findTree(AutomataTree, X, node(_, IsAcc, _)),
    IsAcc, !.                                   %check if X is accepting

%main algorithm
seekAccept([X | ToVisit], AutomataTree, Visited) :-
    \+(findTree(X, Visited, _)), !, %X was not visited
    findTree(AutomataTree, X, node(_, _, Funcs)), !,   %get functions of X
    findall(To, member(edge(_,To), Funcs), Edges), !,  %map edges to states
    append(Edges, ToVisit, NewToVisit), !,     %all neighbours are to be visited
    createNode(Visited, node(X, nil, nil), XVisited), !, %mark X as visited
    seekAccept(NewToVisit, AutomataTree, XVisited). %go next

%++++++++++++++++++++++++++++++++++ EMPTY +++++++++++++++++++++++++++++++++++++%
empty(A):- 
    correct(A, Automata), !, %A must be correct dfa
    %L(A) is empty iff we cant reach accepting state from starting state
    \+(seekAccept(Automata)). 

%+++++++++++++++++++++++++++++++++ EQUAL ++++++++++++++++++++++++++++++++++++++%
%equality on sets is just them being subsets of each other
equal(A1, A2):- 
    subsetEq(A1, A2), !, 
    subsetEq(A2, A1).

%=============================== SUBSETEQ SECTION==============================%
%--------------------------------- COMPLIMENT ---------------------------------%
%compliment(+AutomataTree, -Compliment)
%Given (AutomataTree - bst structured automata)
%Returns (Compliment - bst structured compliment of AutomataTree)

%compliment of empty is empty (doesn't really break anything)
compliment(nil, nil).

%both cases are just going through tree and inverting IsAcc values
compliment(bst(node(X, true, Funcs), Left, Right), 
           bst(node(X, false, Funcs), Compliment1, Compliment2)) :-
    compliment(Left, Compliment1), !,
    compliment(Right, Compliment2), !.

compliment(bst(node(X, false, Funcs), Left, Right), 
           bst(node(X, true, Funcs), Compliment1, Compliment2)) :-
    compliment(Left, Compliment1), !,
    compliment(Right, Compliment2), !.

%--------------------------------- BUILD STATES -------------------------------%
%buildStates(+AutomataTree1, +AutomataTree2, -StateTree)
%Given (AutomataTree1 - bst structured automata) and (AutomataTree2 - -||-)
%Returns (StateTree - bst structured intersection 
%            of AutomataTree1 and AutomataTree2 with empty transition functions)
buildStates(AutomamataTree1, AutomamataTree2, StateTree) :-
    %invoke buildStates/4
    buildStates(AutomamataTree1, AutomamataTree2, nil, StateTree).

%buildStates/4 is buildStates/3 with accumulator tree and traverses first tree
%base accumulator case
buildStates(nil, _, Tree, Tree).
%traversal predicate
buildStates(bst(node(X, IsAcc, _), Left1, Right1), 
            AutomamataTree2, 
            OldTree, StateTree) :-
    buildStates(Left1, AutomamataTree2, OldTree, NewTree1), !, %go left
    buildStates(Right1, AutomamataTree2, NewTree1, NewTree2), !, %go right
    %invoke buildStates/4 with node() to actually build StateTree
    buildStates(node(X, IsAcc, _), AutomamataTree2, NewTree2, StateTree).

%buildStates/4 with node() traverses the second tree to build all 
%   pairs with node()
%base accumulator case
buildStates(node(_, _, _), nil, Tree, Tree).

%traversal predicates
%case: IsAcc is matchcing
buildStates(node(X, IsAcc, _),
            bst(node(Y, IsAcc, _), Left2, Right2), 
            OldTree, StateTree) :-
    buildStates(node(X, IsAcc, _), Left2, OldTree, NewTree1), !, %go left
    buildStates(node(X, IsAcc, _), Right2, NewTree1, NewTree2), !, %go right
    %insert pair node in accumulator tree; if both are accepting 
    %then pair state is also accepting
    createNode(NewTree2, node(pair(X, Y), IsAcc, nil), StateTree). 
%case: IsAccs don't match so pair state is not accepting 
buildStates(node(X, IsAcc1, _),
            bst(node(Y, IsAcc2, _), Left2, Right2), 
            OldTree, StateTree) :-
    IsAcc1 \= IsAcc2, %ensure they dont match
    buildStates(node(X, IsAcc1, _), Left2, OldTree, NewTree1), !, %go left
    buildStates(node(X, IsAcc1, _), Right2, NewTree1, NewTree2), !, %go right
    %insert pair node in accumulator tree; pair state is not accepting 
    createNode(NewTree2, node(pair(X, Y), false, nil), StateTree).

%------------------------------- BUILD FUNCS ----------------------------------%
%buildFuncs(+AutomataTree1, +AutomataTree2, +AutomataTree3, -FuncsTree)
%Given (AutomataTree1 - bst structured 
%                       intersection of AutomataTree2 and AutomataTree3)
%      (AutomataTree2 - bst structured automata)
%      (AutomataTree3 - -||-)
%Returns (FuncsTree - AutomataTree1 with correct transition functions)
buildFuncs(AutomataTree1, AutomataTree2, AutomataTree3, FuncsTree) :-
    %invoke buildFuncs/5
    buildFuncs(AutomataTree1, AutomataTree1, 
                    AutomataTree2, AutomataTree3, FuncsTree).

%buildFuncs/5
%base case 
buildFuncs(nil, _, _, _, nil).
%traversal predicate
%SameTree is full version of tree that we traverse
buildFuncs(bst(node(X, IsAcc, _), Left, Right), SameTree,  
                    AutomataTree1, AutomataTree2, FuncsTree) :-
    buildFuncs(Left, SameTree, 
                    AutomataTree1, AutomataTree2, Left1), !, %go left
    buildFuncs(Right, SameTree,
                    AutomataTree1, AutomataTree2, Right1), !, %go right
    %invoke buildFuncs/5 with node() to fill that node's funcs
    buildFuncs(node(X, IsAcc, []), SameTree, 
                    AutomataTree1, AutomataTree2, NewNode), !,
    %FuncsTree is a result of all 3 predicates
    FuncsTree = bst(NewNode, Left1, Right1).

%buildFuncs/5 with node()
%base case 
buildFuncs(node(X, Y, Z), nil, _, _, node(X, Y, Z)).                
%traverse the tree that the node is a part of to check every possible transition
buildFuncs(node(pair(A, B), IsAcc, L), bst(node(pair(X, Y), _, _), Left, Right), 
                    AutomataTree1, AutomataTree2, NewNode) :-
    buildFuncs(node(pair(A, B), IsAcc, L), Left, 
                            AutomataTree1, AutomataTree2, NewNode1), !,%go left
    buildFuncs(NewNode1, Right, 
                            AutomataTree1, AutomataTree2, NewNode2), !,%go right
    %get funcs of first element of pair from its original tree                        
    findTree(AutomataTree1, A, node(_, _, Funcs1)), !,
    %get funcs of second element of pair from its original tree
    findTree(AutomataTree2, B, node(_, _, Funcs2)), !,
    %check condition for transition existence in dfa intersections
    buildFunc(X, Y, Funcs1, Funcs2, NewNode2, NewNode).

%--------------------------------- BUILD FUNC ---------------------------------%
%buildFunc(+P, +Q, +FuncsP, +FuncsQ, +OriginalNode, -UpdatedNode)
%Given (P - first element of target pair)
%      (Q - second element of target pair)
%      (FuncsP - transitions from first pair element of OriginalNode to P)
%      (FuncsQ - transitions from second pair element of OriginalNode to Q)
%      (OriginalNode - node that we test the condition on)
%Returns (UpdatedNode - OriginalNode with added transition to pair(P, Q) 
%                                                       if condition is met)
buildFunc(P, Q, FuncsP, FuncsQ, node(X, IsAcc, L), node(X, IsAcc, L1)) :-
    %get all letters that go to P
    findall(Letter1, member(edge(Letter1, P), FuncsP), Letters1),
    %get all letters that go to Q
    findall(Letter2, member(edge(Letter2, Q), FuncsQ), Letters2),
    %get the intersection of Letters1 and Letters2 and map to edges to pair(P,Q)
    findall(edge(Letter, pair(P, Q)), 
                (member(Letter, Letters1), member(Letter, Letters2)), Results),
    %append the results
    append(Results, L, L1).

%check failed we do nothing
buildFunc(_, _, _, _, node(X, IsAcc, L), node(X, IsAcc, L)).

%--------------------------------- INTERSEC -----------------------------------%
%intersec(+AutomataTree1, +AutomataTree2, +Start1, +Start2, -IntersecAutomata)
%Given (AutomataTree1 - bst structured automata) and (AutomataTree2 - -||-)
%   and (Start1 - starting state of AutomataTree1) and (Start2 - -||- ...2)
%Returns (IntersecAutomata - intersection of both automata)

%starting state of intersection is just a pair of starting states
intersec(AutomamataTree1, AutomamataTree2, Start1, Start2, 
                        automata(pair(Start1, Start2), [], Intersection1)) :-
    %invoke intersec/3
    intersec(AutomamataTree1, AutomamataTree2, Intersection1).

%intersec/3 is the same as intersec/5 but without starting states
%and returns just the AutomataTree
intersec(AutomamataTree1, AutomamataTree2, AutomamataTree3) :-
    %build intersection with empty trasition functions
    buildStates(AutomamataTree1, AutomamataTree2, StateTree), !,
    %fill transition functions
    buildFuncs(StateTree, AutomamataTree1, AutomamataTree2, AutomamataTree3).

%++++++++++++++++++++++++++++++++++ SUBSETEQ ++++++++++++++++++++++++++++++++++%
subsetEq(A1, A2):- 
    %both A1 and A2 must be correct
    correct(A1, automata(Start1, Alphabet, AutomataTree1)), !,
    correct(A2, automata(Start2, Alphabet, AutomataTree2)), !,
    %A1 is subset of A2 iff L(A1 n A2') is non-empty
    compliment(AutomataTree2, B2), !,
    intersec(AutomataTree1, B2, Start1, Start2, C), !,
    \+(seekAccept(C)).