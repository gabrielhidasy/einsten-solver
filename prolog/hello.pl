:- write('Write sem nome só funciona').
hello :- write('Assim define um nome').
sum_two :- leNumero(A,B), S is A+B, imprimeNumero(S).		

%Lendo na documentação, backtrack é legal
    
leNumero(A,B) :-
    write('Enter first number followed by dot: '),
    read(A),
    write('Enter second number: '),
    read(B).

imprimeNumero(S) :- write(S), write(' is the sum').

fatorial(N) :-
    S is 1,
    fatorialaux(N, 1, S), 
    write(S).	

fatorialaux(N,B,S) :-
    write(S), write("\n"),
    N > 0,
    N1 is N-1,
    S1 is S*B,
    B1 is B+1,
    fatorialaux(N1,B1,S1).


% = == is, e % é comentario
fibonacci(N) :-
    fibonaccia(N,1,1).

fibonaccia(N,A,B) :-
    A1 is A+B,
    B1 is A1+B,
    N > 1,
    N1 is N-1,
    write(A), write("\n"),
    write(B), write("\n"),
    fibonaccia(N1,A1,B1).

% com :- voce define uma regra, com :? uma query,
%o poder vem de combinar queries com , (e), e mais
%ainda, de perguntar coisas como gosta(pedro,X),
%gosta(maria,X), ele tenta um X pra pedro, e vê se
%da em Maria, se não backtrack
%colorcar só nome(coisa). cria um fato
%nome(coisa) :- ... . uma regra
