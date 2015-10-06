start(F,C,D) :-
    open(F, read, In),
    read_lines(In,1,[],DLines),
    read_all_domains(DLines,[],D),
    read_lines(In,1,[],CLines),
    close(In),
    %writeln(CLines),
    read_all_rules(CLines,[],C),!.
    %writeln(D),
    %writeln(D),
    %writeln(C).

read_left_rule(S,R) :-
    %writeln("Parsing left"),
    length(S,N),
    (N == 3 -> [O1,_,O2] = S, R = ['>',O1,O2]).
read_right_rule(S,R) :-
    %writeln("Parsing right"),
    length(S,N),
    (N == 3 -> [O1,_,O2] = S, R = ['<',O1,O2]).
read_or_rule(S,R) :-
    %writeln("Parsing or"),
    %writeln(S),
    [Name,_,P1,_Or,Name2,_,P2|_] = S,
    (atom_number(P1,PP1) -> RP1 = PP1; RP1 = P1),
    (atom_number(P2,PP2) -> RP2 = PP2; RP2 = P2),
    %writeln(Name),
    %writeln(P1),
    %writeln(REST),
    R = ['or',['=',Name,RP1],['=',Name2,RP2]].
read_abs_rule(S,R) :-
    %writeln("Parsing abs"),
    [SH|_] = S,
    split_atom(' ',SH,SSH),
    (member('+',SSH) -> read_pos_abs_rule(S,R);
     read_neg_abs_rule(S,R)).
    %writeln(R).
read_pos_abs_rule(S,R) :-
    [SH,SL] = S,
    split_atom(' ',SH,S1),
    split_atom(' ',SL,S2),
    [N1,_OP,_|_] = S1,
    [_,N2|_] = S2,
    (atom_number(N2,NN2) ->
	 R = ['=',['abs',['+',N1,1]],NN2];
     R = ['=',['abs',['+',N1,1]],N2]).
    
read_neg_abs_rule(S,R) :-
    %writeln('Parsing negative abs'),
    [SH,SL] = S,
    split_atom(' ',SH,S1),
    split_atom(' ',SL,S2),
    [N1,_OP,N2|_] = S1,
    [_,X] = S2,
    atom_number(X,XX),
    (atom_number(N2,NN2) ->
	 R = ['=',['abs',['-',N1,NN2]],XX];
     R = ['=',['abs',['-',N1,N2]],XX]).
read_plus_equal_rule(S,R) :-
    %write('Parsing positive equal'),
    %writeln(S),
    ([N1,'+',X,_,N2] = S ->
	 atom_number(X,XX),
	 (atom_number(N1,NN1) ->
	      (atom_number(N2,NN2) ->
		   R = ['=',['+',NN1,XX],NN2];
	       R = ['=',['+',NN1,XX],N2]);
	  (atom_number(N2,NN2) ->
	       R = ['=',['+',N1,XX],NN2];
	   R = ['=',['+',N1,XX],N2]));
     [N1,_,N2,'+',X] = S,
     atom_number(X,XX),
     (atom_number(N2,NN2) ->
	  R = ['=',N1,['+',NN2,XX]];
      R = ['=',N1,['+',N2,XX]])).

read_minus_equal_rule(S,R) :-
    %write('Parsing negative equal'),
    %writeln(S),
    ([N1,'-',X,_,N2] = S ->
	 atom_number(X,XX),
	 (atom_number(N2,NN2) ->
	      R = ['=',['-',N1,XX],NN2];
	  R = ['=',['-',N1,XX],N2]);
     [N1,_,N2,'-',X] = S,
     atom_number(X,XX),
     (atom_number(N1,NN1) ->
	  (atom_number(N2,NN2) ->
	       R = ['=',NN1,['-',NN2,XX]];
       R = ['=',NN1,['-',N2,XX]]);
      (atom_number(N2,NN2) ->
	   R = ['=',N1,['-',NN2,XX]];
       R = ['=',N1,['-',N2,XX]]))).

read_real_equal_rule(S,R) :-
    %write('Parsing real equal'),
    %writeln(S),
    [N1,_,N2] = S,
    %atom_number(N2,NN2),
    (atom_number(N1,NN1) ->
	 (atom_number(N2,NN2) -> R = ['=',NN1,NN2];
	  R = ['=',NN1,N2]);
     (atom_number(N2,NN2) -> R = ['=',N1,NN2];
      R = ['=',N1,N2])).
    %writeln(NN2).

read_equal_rule(S,R):-
    %writeln('Parsing equal'),
    (member('+',S) -> read_plus_equal_rule(S,R);
     (member('-',S) -> read_minus_equal_rule(S,R);
      read_real_equal_rule(S,R))).
    
read_rule(Rulel,R) :-
    [Rule|_] = Rulel,
    split_atom('|',Rule,ABS),
    length(ABS,ISABS),
    %writeln(ABS),
    split_atom(' ',Rule,S),
    (member('>',S) -> read_left_rule(S,R);
     (member('<',S) -> read_right_rule(S,R);
      (member('or',S) -> read_or_rule(S,R); 
       (ISABS == 2 -> read_abs_rule(ABS,R);
	(member('=',S) -> read_equal_rule(S,R);true))))).
    %writeln(R).

read_all_rules([],R,RR) :- reverse(R,RR).
read_all_rules(CLines,Acc,C) :-
    [H|T] = CLines,
    read_rule(H,X),
    append([X],Acc,Acc1),
    read_all_rules(T,Acc1,C).

read_all_domains([],R,RR) :- reverse(R,RR).
read_all_domains(DLines,Acc,R) :-
    [[H]|T] = DLines,
    read_domain_line(H,X),
    append([X],Acc,Acc1),
    read_all_domains(T,Acc1,R).
    
    
read_domain_line(DLine,DElement) :-
    %writeln(DLine),
    string_to_atom(DLine, InputA),
    split_atom(' ',InputA, InputAS),
    [DOMAINA|REST]=InputAS,
    split_atom(':', DOMAINA, [DOMAIN|_]),
    split_domain_list(REST,[],DR),
    append([domain(DOMAIN)],DR,DElement).

split_domain_list([],R,RE) :- reverse(R, RE).
split_domain_list(IN,Acc,R) :-
    [H|T] = IN,
    (T == [] -> split_atom('.', H, X); split_atom(',', H, X)),
    append(X, Acc, Acc1),
    split_domain_list(T, Acc1, R).

split_atom(S, A, L) :- atomic_list_concat(XL, S, A), delete(XL, '', L).

read_single_line(_,0,R,Line) :-
    reverse(R,Linel),
    string_to_list(Line,Linel).

read_single_line(In,1,Acc,R) :-
    get_char(In,C),
    (C == end_of_file ->
	 read_single_line(_,0,Acc,R);
     (C == '\n' ->
	  read_single_line(_,0,Acc,R);
      (C == '.'  ->
	   read_single_line(_,0,Acc,R);
       (C == '\'' -> read_single_line(In,1,Acc,R);
	append([C],Acc,Acc1),
	read_single_line(In,1,Acc1,R))))).
     

read_lines(_,0,CR,CLines) :-
    reverse(CR,CLines).
read_lines(In,1,CAcc,CLines) :-
    read_single_line(In,1,[],Line),
    string_length(Line,N),
    append([[Line]], CAcc, CAcc1),
    (N == 0 -> read_lines(_,0,CAcc,CLines);
     read_lines(In,1,CAcc1,CLines)).
