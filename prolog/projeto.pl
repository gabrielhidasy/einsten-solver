select(A,S):- select(A,S,_).

r_left_of(A,B,C):- append(_,[A,B|_],C).

r_right_of(A,B,C):- r_left_of(B,A,C).

left_of(L,R,[L|T]) :-
    member(R,T).
left_of(L,R,[_|T]) :-
    left_of(L,R,T).

right_of(R,L,A):-
    left_of(L,R,A).
    
next_to(A,B,C):- r_left_of(A,B,C).
next_to(A,B,C):- r_left_of(B,A,C).

between(A,B,C,R) :-
    left_of(A,B,R),
    left_of(B,C,R).
between(A,B,C,R) :-
    right_of(A,B,R),
    right_of(B,C,R).

%Para garantir maior desempeenho, primeiro as select, depois
%as r_*, depois as next_to e só depois as left, right e between

solve(R) :-
    %Essa tem que usar C pra criar a serie de selects e outras abaixo
    R = [h(c1,_,_,_,_,_,_),h(c2,_,_,_,_,_,_),h(c3,_,_,_,_,_,_),
	 h(c4,_,_,_,_,_,_),h(c5,_,_,_,_,_,_)],
    %basquete = 4
    select(h(c4,_,_,_,_,_,basquete),R),
    %futebol = 2
    select(h(c2,_,_,_,_,_,futebol),R),
    %uva = corrida
    select(h(_,_,_,_,_,uva,corrida),R),
    %junior = verde
    select(h(_,verde,junior,_,_,_,_),R),
    %musica = vermelha
    select(h(_,vermelha,_,musica,_,_,_),R), 
    %'32anos' = 2
    select(h(c2,_,_,_,'32anos',_,_),R),
    %canto = 1 or canto = 5
    (select(h(c1,_,_,canto,_,_,_),R);select(h(c5,_,_,canto,_,_,_),R)),

    %canto - 1 = junior
    r_right_of(h(_,_,_,canto,_,_,_),h(_,_,junior,_,_,_,_),R),
    %malabarismo + 1 = canto
    r_left_of(h(_,_,_,malabarismo,_,_,_),h(_,_,_,canto,_,_,_),R),
    %futebol - 1 = imitacao
    r_right_of(h(_,_,_,_,_,_,futebol),h(_,_,_,imitacao,_,_,_),R),
    %alexander + 1 = '32anos'
    r_left_of(h(_,_,alexander,_,_,_,_),h(_,_,_,_,'32anos',_,_),R),
    %abacaxi - 1 = natacao
    r_right_of(h(_,_,_,_,_,abacaxi,_),h(_,_,_,_,_,_,natacao),R),
    %michel + 1 = boliche
    r_left_of(h(_,_,michel,_,_,_,_),h(_,_,_,_,_,_,boliche),R),
    %michel + 1 = maca
    r_left_of(h(_,_,michel,_,_,_,_),h(_,_,_,_,_,maca,_),R),
    %'22anos' + 1 = michel
    r_left_of(h(_,_,_,_,'22anos',_,_),h(_,_,michel,_,_,_,_),R),
    %canto - 1 = junior
    r_right_of(h(_,_,_,canto,_,_,_),h(_,_,junior,_,_,_,_),R),

    %|morango - abacaxi| = 1
    next_to(h(_,_,_,_,_,morango,_),h(_,_,_,_,_,abacaxi,_),R),
    %|branca - morango| = 1
    next_to(h(_,_,_,_,_,morango,_),h(_,branca,_,_,_,_,_),R),    
    %|'35anos' - amarela| = 1
    next_to(h(_,_,_,_,'35anos',_,_),h(_,amarela,_,_,_,_,_),R),

    
    %basquete > '22anos'
    right_of(h(_,_,_,_,_,_,basquete),h(_,_,_,_,'22anos',_,_),R),
    %basquete < corrida
    left_of(h(_,_,_,_,_,_,basquete),h(_,_,_,_,_,_,corrida),R),
    %vermelha < '24anos'
    left_of(h(_,vermelha,_,_,_,_,_),h(_,_,_,_,'24anos',_,_),R),
    %malabarismo < canto
    left_of(h(_,_,_,malabarismo,_,_,_),h(_,_,_,canto,_,_,_),R),
    %futebol > imitacao
    right_of(h(_,_,_,_,_,_,futebol),h(_,_,_,imitacao,_,_,_),R),
    %rodolfo > vermelha
    right_of(h(_,_,rodolfo,_,_,_,_),h(_,vermelha,_,_,_,_,_),R),
    %alexander < '32anos'
    left_of(h(_,_,alexander,_,_,_,_),h(_,_,_,_,'32anos',_,_),R),

    domain_contraints(R),!.

domain_contraints(R) :-
    %Essa tem que receber a lista D e executar selects
    %como essas ai
    %camisa: amarela, azul, branca, verde, vermelha
    select(h(_,amarela,_,_,_,_,_),R),
    select(h(_,azul,_,_,_,_,_),R),
    select(h(_,branca,_,_,_,_,_),R),
    select(h(_,verde,_,_,_,_,_),R),
    select(h(_,vermelha,_,_,_,_,_),R),

    %nome: alexander, heitor, junior, michel, rodolfo
    select(h(_,_,alexander,_,_,_,_),R),
    select(h(_,_,heitor,_,_,_,_),R),
    select(h(_,_,junior,_,_,_,_),R),
    select(h(_,_,michel,_,_,_,_),R),
    select(h(_,_,rodolfo,_,_,_,_),R),
    
    %talento: canto, danca, imitacao, malabarismo, musica
    select(h(_,_,_,canto,_,_,_),R),
    select(h(_,_,_,danca,_,_,_),R),
    select(h(_,_,_,imitacao,_,_,_),R),
    select(h(_,_,_,malabarismo,_,_,_),R),
    select(h(_,_,_,musica,_,_,_),R),

    %idade: '22anos', '24anos', '29anos', '32anos', '35anos'
    select(h(_,_,_,_,'22anos',_,_),R),
    select(h(_,_,_,_,'24anos',_,_),R),
    select(h(_,_,_,_,'29anos',_,_),R),
    select(h(_,_,_,_,'32anos',_,_),R),
    select(h(_,_,_,_,'35anos',_,_),R),

    %fruta: abacaxi, banana, maca, morango, uva
    select(h(_,_,_,_,_,abacaxi,_),R),
    select(h(_,_,_,_,_,banana,_),R),
    select(h(_,_,_,_,_,maca,_),R),
    select(h(_,_,_,_,_,morango,_),R),
    select(h(_,_,_,_,_,uva,_),R),
    
    %esporte: basquete, boliche, corrida, futebol, natacao
    select(h(_,_,_,_,_,_,basquete),R),
    select(h(_,_,_,_,_,_,boliche),R),
    select(h(_,_,_,_,_,_,corrida),R),
    select(h(_,_,_,_,_,_,futebol),R),
    select(h(_,_,_,_,_,_,natacao),R),
    !.
    
split_terms(D) :-
    [HEAD|TAIL] = D,
    insert_terms(HEAD),
    split_terms(TAIL).

split_terms(_).

insert_terms(TERM) :-
    [NAME|DATA] = TERM,
    assertz(NAME),
    insert_terms_aux(NAME,DATA).

insert_terms_aux(NAME,DATA) :-
    [T1|TR] = DATA,
    %The two lines below extract the name from the functor domain and
    %Put then to game
    NAME =.. [_Functor, N],
    TERM =.. [N,T1],
    assertz(TERM),
    insert_terms_aux(NAME,TR).

insert_terms_aux(_,_).
 
