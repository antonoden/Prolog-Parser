/******************************************************************************/
/* Prolog Pascal-Parser - within course DVGC01 at Karlstad University         */
/*    Programmed by Anton Odén                                                */
/*    Helped by coursematerial from lectures including skeleton code from:    */    
/*    1. (skeletonparser.pl): Prolog Lab 2 example - Grammar test bed         */
/*    2. (cmreader.pl): From Programming in Prolog (4th Ed.) Clocksin &       */
/*          Mellish, Springer (1994) Chapter 5, pp 101-103 (DFR (140421)      */
/*          modified for input from a file)                                   */
/******************************************************************************/

/******************************************************************************/
/* Grammar Rules in Definite Clause Grammar form                              */
/* This the set of productions, P, for this grammar                           */
/* This is a slightly modified from of the Pascal Grammar for Lab 2 Prolog    */
/******************************************************************************/

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/*      PARSER                                                                */
/******************************************************************************/

parser(TList, Res) :- (program(TList, Res), Res = [], write('Parse OK!'));
                        write('Parse Fail!').

program       --> prog_head, var_part, stat_part.

/******************************************************************************/
/* Program Header                                                             */
/******************************************************************************/

/* [program], [id], [(], [input], [,], [output], [)], [;]. */ 
prog_head      --> [256],[270],[40],[257],[44],[258],[41],[59]. 

/******************************************************************************/
/* Var_part                                                                   */
/******************************************************************************/

% [var part]	::=	var [var dec list]
var_part       --> [259], var_dec_list.

% [var dec list]	::=	[var dec] | [var dec list] [var dec]
var_dec_list   --> var_dec | var_dec, var_dec_list.

% [var dec]	::=	[id list] : [type] ;
var_dec        --> id_list, [58], type, [59].

% [id list]	::=	id | [id list] , id
id_list        --> [270] | [270], [44], id_list.

% [type]	::=	integer | real | boolean
type           --> [260] | [263] | [264].

/******************************************************************************/
/* Stat part                                                                  */
/******************************************************************************/

% [stat part]	::=	begin [stat list] end .
stat_part      --> [261], stat_list, [262], [46].

% [stat list]	::=	[stat] | [stat list] ; [stat]
stat_list      --> stat | stat, [59], stat_list.

% [stat]	::=	[assign stat]
stat           --> assign_stat.

% [assign stat]	::=	id := [expr]
assign_stat    --> [270], [271], expr.

% [expr]	::=	[term] | [expr] + [term]
expr           --> term | term, [42], expr.

% [term]	::=	[factor] | [term] * [factor]
term           --> factor | factor, [43], term.

% [factor]	::=	( [expr] ) | [operand]
factor         --> operand | [40], expr, [41].

% [operand]	::=	id | number
operand        --> [270] | [272].

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/*  LEXER                                                                     */
/******************************************************************************/

lexer([], TOK) :- TOK = []. 

lexer([H|T], [HTOK|TTOK]) :- match(H, HTOK), lexer(T, TTOK). % keywords
 
match(L, T) :- L='(',         char_code(L, T).
match(L, T) :- L=')',         char_code(L, T).
match(L, T) :- L='+',         char_code(L, T).
match(L, T) :- L='*',         char_code(L, T).
match(L, T) :- L=',',         char_code(L, T).
match(L, T) :- L=';',         char_code(L, T).
match(L, T) :- L=':',         char_code(L, T).
match(L, T) :- L='.',         char_code(L, T).
match(L, T) :- L='program',     T is 256.
match(L, T) :- L='input',       T is 257.
match(L, T) :- L='output',      T is 258.
match(L, T) :- L='var',         T is 259.
match(L, T) :- L='integer',     T is 260.
match(L, T) :- L='begin',       T is 261.
match(L, T) :- L='end',         T is 262.
match(L, T) :- L='boolean',     T is 263.
match(L, T) :- L='real',        T is 264.
%match(L, T) :- L='notdef',      T is 265.
%match(L, T) :- L='notdef',      T is 266.
%match(L, T) :- L='notdef',      T is 267.
%match(L, T) :- L='notdef',      T is 268.
%match(L, T) :- L='notdef',      T is 269.
match(L, T) :- L=':=',          T is 271.
%match(L, T) :- L='notdef',      T is 273.
%match(L, T) :- L='notdef',      T is 274.
match(L, T) :- name(L, [H|Tail]), char_type(H, digit),
               match_num(Tail), T is 272.
match(L, T) :- name(L, [H|Tail]), char_type(H, alpha),
                match_id(Tail), T is 270.
match(L, T) :- L= -1,           T is 275.
match(_, T) :-                  T is 273.

match_num([]).
match_num([H|T]) :- char_type(H, digit), match_num(T).

match_id([]).
match_id([H|T]) :- char_type(H, alnum), match_id(T).

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/*      READER                                                                */
/******************************************************************************/

read_in(File,[W|Ws]) :- see(File), get0(C), 
                        readword(C, W, C1), restsent(W, C1, Ws), nl, seen.

/******************************************************************************/
/* Given a word and the character after it, read in the rest of the sentence  */
/******************************************************************************/

restsent(W, _, [])         :- W = -1.                /* added EOF handling */
restsent(W, _, [])         :- lastword(W).
restsent(_, C, [W1 | Ws ]) :- readword(C, W1, C1), restsent(W1, C1, Ws).

/******************************************************************************/
/* Read in a single word, given an initial character,                         */
/* and remembering what character came after the word (NB!)                   */
/******************************************************************************/

readword(C, W, _)  :- C = -1, W = C.                    /* 'EOF' handling */
readword(C, W, C2) :- C = 58, get0(C1), readwordaux1(C, W, C1, C2).  /* ':=' handling   */
readword(C, W, C1) :- single_character( C ), name(W, [C]), get0(C1). /* singlechar handling */
readword(C, W, C2) :-      /* number handling */ 
   char_type(C, digit), 
   get0(C1), 
   readwordaux2(C1, Cs, C2),
   name(W, [C|Cs]).

readword(C, W, C2) :-
   in_word(C, NewC ),
   get0(C1),
   restword(C1, Cs, C2),
   name(W, [NewC|Cs]).

readword(_, W, C2) :- get0(C1), readword(C1, W, C2).

restword(C, [NewC|Cs], C2) :-
   in_word(C, NewC),
   get0(C1),
   restword(C1, Cs, C2).

restword(C, [ ], C).

/* Help to check if word is assign (':=') */
readwordaux1(C, W, C1, C2) :-  C1 \= 61, name(W, [C]), C1 = C2.
readwordaux1(C, W, C1, C2) :-  C1 = 61, name(W, [C, C1]), get0(C2).

/* Helps to construct a number */
readwordaux2(C, [C,Cs], C2) :- char_type(C, digit), get0(C1), readwordaux2(C1, Cs, C2).
readwordaux2(C, [ ], C).

/******************************************************************************/
/* These characters form words on their own                                   */
/******************************************************************************/

single_character(40).                  /* ( */
single_character(41).                  /* ) */
single_character(42).                  /* + */
single_character(43).                  /* * */
single_character(44).                  /* , */
single_character(45).                  /* - */
single_character(46).                  /* . */
single_character(58).                  /* : */
single_character(59).                  /* ; */
single_character(61).                  /* = */

/******************************************************************************/
/* These characters can appear within a word.                                 */
/* The second in_word clause converts character to lower case                 */
/******************************************************************************/

in_word(C, C) :- C>96, C<123.             /* a b ... z */
in_word(C, L) :- C>64, C<91, L is C+32.   /* A B ... Z */
in_word(C, C) :- C>47, C<58.              /* 1 2 ... 9 */

/******************************************************************************/
/* These characters can appear within a number.                               */
/******************************************************************************/

in_number(C, C) :- C>47, C<58.              /* 1 2 ... 9 */

/******************************************************************************/
/* These words terminate a sentence                                           */
/******************************************************************************/

lastword('.').

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/*  TESTFUNCTIONS                                                             */
/******************************************************************************/

run_all_tests() :- 
   tell('parser.out'),
   write('Testing OK programs '), nl,
   run_specific_test('testok1.pas'),
   run_specific_test('testok2.pas'), 
   run_specific_test('testok3.pas'), 
   run_specific_test('testok4.pas'), 
   run_specific_test('testok5.pas'), 
   run_specific_test('testok6.pas'), 
   run_specific_test('testok7.pas'),
   nl, write('Testing a-z programs '), nl,
   run_specific_test('testa.pas'),
   run_specific_test('testb.pas'),
   run_specific_test('testc.pas'),
   run_specific_test('testd.pas'),
   run_specific_test('teste.pas'),
   run_specific_test('testf.pas'),
   run_specific_test('testg.pas'),
   run_specific_test('testh.pas'),
   run_specific_test('testi.pas'),
   run_specific_test('testj.pas'),
   run_specific_test('testk.pas'),
   run_specific_test('testl.pas'),
   run_specific_test('testm.pas'),
   run_specific_test('testn.pas'),
   run_specific_test('testo.pas'),
   run_specific_test('testp.pas'),
   run_specific_test('testq.pas'),
   run_specific_test('testr.pas'),
   run_specific_test('tests.pas'),
   run_specific_test('testt.pas'),
   run_specific_test('testu.pas'),
   run_specific_test('testv.pas'),
   run_specific_test('testw.pas'),
   run_specific_test('testx.pas'),
   run_specific_test('testy.pas'),
   run_specific_test('testz.pas'),
   nl, write('Testing fun programs '), nl,
   run_specific_test('fun1.pas'),
   run_specific_test('fun2.pas'),
   run_specific_test('fun3.pas'),
   run_specific_test('fun4.pas'),
   run_specific_test('fun5.pas'),
   nl, write('Testing sem programs '), nl,
   run_specific_test('sem1.pas'),
   run_specific_test('sem2.pas'),
   run_specific_test('sem3.pas'),
   run_specific_test('sem4.pas'),
   run_specific_test('sem5.pas'),    
   told.

run_specific_test(F) :-
   atom_concat('testfiles/', F, FP),
   atom_concat('Testing ', FP, START),
   nl, write(START), nl,
   read_in(FP, Ls), 
   write(Ls), nl,
   lexer(Ls, Ts),
   write(Ts), nl,
   parser(Ts, _),
   nl, atom_concat(FP, ' end of parse', END),
   write(END), nl.	

/******************************************************************************/
/* End of program                                                             */
/******************************************************************************/
