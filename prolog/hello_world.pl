:- write('Write sem nome só funciona').
hello :- write('Assim define um nome').
sum_two :- leNumero(A,B), S = A+B, imprimeNumero(S).
fatorial :- write("ola").			
% = == is, e % é comentario

leNumero(A,B) :-
    write('Enter first blanumber followed by dot: '),
    read(A),
    write('Enter second number: '),
    read(B).

imprimeNumero(S) :- write(S), write(' is the sum').
			
fatorialaux(N,S) :- N < 2, S is S*fatorialaux(N-1,S).
