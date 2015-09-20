:- module(babao,[solver1/0]).
:- use_module(library(clpfd)).
solver(D,C,S) :-
    split_terms(D),
    S is 2+4.
solver1() :-
    D = [[domain(cor), azul, vermelha, preta],
	 [domain(nacionalidade), alemao, espanhol, italiano]],
    C = [[=, espanhol, [+, vermelha, 1]],
	 [=, alemao, azul],
	 [=, italiano, 2]],
    solver(D,C,S),write(S).

split_terms(D) :-
    [HEAD|TAIL] = D,
    insert_terms(HEAD),
    split_terms(TAIL).

split_terms(_).

insert_terms(TERM) :-
    [NAME|DATA] = TERM,
    write(NAME),
    assertz(NAME),
    insert_terms_aux(NAME,DATA).

insert_terms_aux(NAME,DATA) :-
    [T1|TR] = DATA,
    %The two lines below extract the name from the functor domain and
    %Put then to game
    NAME =.. [_Functor, N],
    TERM =.. [N,T1],
    write(TERM),
    assertz(TERM),
    insert_terms_aux(NAME,TR).

insert_terms_aux(_,_).
 
generate_fields(Rows,Ncols) :-  
  append(Rows, Vs), Vs ins 1..Ncols,
  maplist(all_distinct, Rows),
  maplist(label, Rows).      

