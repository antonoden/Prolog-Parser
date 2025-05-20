/* Exercise 2.1. Write a Prolog predicate analyse_list/1 that takes a list as its argu-
ment and prints out the lists head and tail on the screen. If the given list is empty, the
predicate should put out an message reporting this fact. If the argument term isnt a
list at all, the predicate should just fail. Examples:
?- analyse_list([dog, cat, horse, cow]).
This is the head of your list: dog
This is the tail of your list: [cat, horse, cow]
Yes
?- analyse_list([]).
This is an empty list.
Yes
?- analyse_list(sigmund_freud).
No */

analyse_list([]) :-
  write('This is amn empty list.'), nl.

analyse_list([H|T]) :-
  write('This is the head of your list: '), write(H), nl,
  write('This is the tail of your list: '), write(T), nl.

/* Exercise 2.2. Write a Prolog predicate membership/2 that works like the built-in
predicate member/2 (without using member/2).
Hint: This exercise, like many others, can and should be solved using a recursive approach
and the head/tail-pattern for lists.
*/

membership(V, [H|T]) :-
    V == H.

membership(V, [H|T]) :-
    membership(V, T).
  

/* Exercise 2.3. Implement a Prolog predicate remove_duplicates/2 that removes all
duplicate elements from a list given in the ﬁrst argument and returns the result in the
second argument position. Example:
?- remove_duplicates([a, b, a, c, d, d], List).
List = [b, a, c, d]
Yes */

/* remove duplicates function
Inparam: 1
Outparam: 2 
*/
remove_duplicates([], L) :-  
  L = []. 

remove_duplicates([H|T], L) :-
  remove_duplicates_aux(H, T, L),
  remove_duplicates(L, L).

/* Auxiliary remove duplicate function 
Inparam: 1, 2 
Outparam: 3
*/
remove_duplicates_aux(X, [], L) :-
  write('end of list'), write(X), nl, L = [X].

remove_duplicates_aux(X, [H|T], L) :-
  X == H, remove_duplicates_aux(X, T, L).

remove_duplicates_aux(X, [H|T], L) :-
  remove_duplicates_aux(X, T, [H|L]).
  
/* Exercise 2.4. Write a Prolog predicate reverse_list/2 that works like the built-in
predicate reverse/2 (without using reverse/2). Example:
?- reverse_list([tiger, lion, elephant, monkey], List).
List = [monkey, elephant, lion, tiger]
Yes
*/

reverse_list(Xs, Ys) :-
  reverse_list(Xs, [], Ys).

reverse_list([X|Xs], Acc, Ys) :-
  reverse_list(Xs, [X|Acc], Ys).

reverse_list([], Ys, Ys).

/* Exercise 2.5. Consider the following Prolog program:
whoami([]).
20 Chapter 2. List Manipulation
whoami([_, _ | Rest]) :-
whoami(Rest).
Under what circumstances will a goal of the form whoami(X) succeed?
*/

whoami([]).

whoami([_,_ | Rest]) :-
  whoami(Rest).

/* It will succed for even number of atoms in list. Cause if the list is 
uneven there is not possibility to reach the ending function for empty list 
As two values is picked out of list every time. If we would pick out more values 
at header part then we could get true on for examples all lists that has a number
of atoms divided by 3 */

/* Exercise 2.6. The objective of this exercise is to implement a predicate for returning
the last element of a list in two different ways.
(a) Write a predicate last1/2 that works like the built-in predicate last/2 using a
recursion and the head/tail-pattern for lists.
(b) Define a similar predicate last2/2 solely in terms of append/3, without using a
recursion.
*/

last1([H|T], X) :- T == [], X = H.

last1([H|T], X) :- last1(T, X).

/* Exercise 2.7. Write a predicate replace/4 to replace all occurrences of a given 
element (second argument) by another given element (third argument) in a given list 
(first argument). Example:
?- replace([1, 2, 3, 4, 3, 5, 6, 3], 3, x, List).
List = [1, 2, x, 4, x, 5, 6, x]
Yes
*/

replace1([], X, Y, L).

replace1([H|T], X, Y, [Y|T]) :-
    H == X, replace1(T, X, Y, [Y|T]).

replace1([H|T], X, Y, L) :-
    replace1(T, X, Y, [H|T]).

/*Exercise 2.8. Prolog lists without duplicates can be interpreted as sets. Write a
program that given such a list computes the corresponding power set. Recall that the
power set of a set S is the set of all subsets of S. This includes the empty set as well as
the set S itself.
Define a predicate power/2 such that, if the first argument is instantiated with a
list, the corresponding power set (i.e., a list of lists) is returned in the second position.
Example:
?- power([a, b, c], P).
P = [[a, b, c], [a, b], [a, c], [a], [b, c], [b], [c], []]
Yes
Note: The order of the sub-lists in your result doesn’t matter.*/

/*Exercise 3.1. Write a Prolog predicate distance/3 to calculate the distance between
two points in the 2-dimensional plane. Points are given as pairs of coordinates. Examples:
?- distance((0,0), (3,4), X).
X = 5
Yes
?- distance((-2.5,1), (3.5,-4), X).
X = 7.81025
Yes
*/

distance1((AX,AY), (BX,BY), HYP) :-
  C = (AX,BY),
  MOT is abs(AX-(BX)),
  NAR is abs(AY-(BY)),
  HYP is sqrt(MOT**2 + NAR**2).






