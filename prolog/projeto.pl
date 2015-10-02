:- module(bobao,[solver/3]).
%Fazer o leitor de C, D, S

start(C,D) :-
    read_lines(1,[],DLines),
    read_all_domains(DLines,[],D),
    read_lines(1,[],CLines),
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

read_single_line(0,R,Line) :-
    reverse(R,Linel),
    string_to_list(Line,Linel).

read_single_line(1,Acc,R) :-
    get_char(C),
    (C == '\n' ->
	 read_single_line(0,Acc,R);
     (C == '.'  ->
	  read_single_line(0,Acc,R);
      (C == '\'' -> read_single_line(1,Acc,R);
       append([C],Acc,Acc1),
       read_single_line(1,Acc1,R)))).
     

read_lines(0,CR,CLines) :-
    reverse(CR,CLines).
read_lines(1,CAcc,CLines) :-
    read_single_line(1,[],Line),
    string_length(Line,N),
    append([[Line]], CAcc, CAcc1),
    (N == 0 -> read_lines(0,CAcc,CLines);
     read_lines(1,CAcc1,CLines)).

print_S([]).
print_S(S) :-
    [H|T] = S,
    c(Name,Estate) = H,
    write(Name), write(' = '),writeln(Estate),
    print_S(T).

solve(_File,_Sol) :-
    start(C,D),solver(D,C,S),print_S(S),!.

solver(D,C,S) :-
    assertz(con(house,1)),
    assertz(con(house,2)),
    assertz(con(house,3)),
    assertz(con(house,4)),
    assertz(con(house,5)),
    assertz(con(house,6)),
    assertz(con(house,7)),
    assertz(con(house,8)),
    assertz(con(house,9)),
    assertz(con(house,10)),
    parse_D(D,[],RlistD),
    [DH|_] = D,
    length(DH,Dlent),
    Dlen is Dlent - 1,
    make_houses(Dlen,1,[],RHlist),
    reverse(RHlist,Hlist),
    parse_C(C,RlistC,[],[]),
    append(Hlist,RlistC,TempList),
    append(TempList,RlistD,Alist),
    atomList_termList(Alist,[],RTlist,Houses),
    %maplist(writeln, Alist),
    reverse(RTlist,Tlist),
    gera_houses(Houses,2),
    listCall(Tlist),
    %maplist(writeln,Houses),
    create_S(Houses,[],S),
    %print_S(S),
    %printlist(S),
    %Converter essa saida no formato que o meidanis quer
    retractall(con(_,_)),!.

testAll :-
    problema(_X,D,C),solver(D,C,S),maplist(writeln, S),false.

create_S([],R,R).
create_S(Houses,Acc,S) :-
    [H|T] = Houses,
    [HouseN|Components] = H,
    _X-N = HouseN,
    create_S_aux(N, Components, [], R),
    append(R,Acc,Acc1),
    create_S(T,Acc1,S).

create_S_aux(_,[],R,R).

create_S_aux(N,[Component|T],Acc,R) :-
    _HN-TN = Component,
    atom_number(N,Num),
    append([c(TN,Num)],Acc,Acc1),
    
    create_S_aux(N,T,Acc1,R).
    
%-----------------------------------------------------------------------%
%This block parses the domain (D), generating the size of the domain,   %
%and basic rules to conect all domains to the solution                  %
%Acumuladores são usados para extrair toda a informação das listas C e D%
%-----------------------------------------------------------------------%
parse_D(D,Acc,RlistD) :-
    [HEAD|TAIL] = D,
    insert_terms(HEAD,TDRules),
    append(TDRules,Acc,Acc1),
    parse_D(TAIL,Acc1,RlistD).
parse_D(_,R,R).

insert_terms(TERM,TDRules) :-
    [NAME|DATA] = TERM,
    assertz(NAME),
    insert_terms_aux(NAME,DATA,[],TDRules).

insert_terms_aux(NAME,DATA,Acc,TDRules) :-
    [T1|TR] = DATA,
    NAME =.. [_Functor, N],
    TERM =.. [con,N,T1],
    assertz(TERM),
    make_domain_rule(N,T1,Rule),
    append([Rule],Acc,Acc1),
    insert_terms_aux(NAME,TR,Acc1,TDRules).

insert_terms_aux(_,_,R,R).

make_domain_rule(N,T,Rule) :-
    atom_concat('one_of(Houses, [',N,Rule0),
    atom_concat(Rule0,'-\'',Rule1),
    atom_concat(Rule1,T,Rule2),
    atom_concat(Rule2,'\' , ',Rule3),
    atom_concat(Rule3,'_',Rule4),
    atom_concat(Rule4,'])',Rule).

make_houses(N,N,R,R).
make_houses(N,B,Acc,HRules) :-
    B1 is B+1,
    atom_concat('two_of(Houses, r_left_of, [[house-\'',B,Rule0),
    atom_concat(Rule0,'\'] , [house-\'',Rule1),
    atom_concat(Rule1,B1,Rule2),
    atom_concat(Rule2,'\']])',Rule),
    append([Rule],Acc,Acc1),
    make_houses(N,B1,Acc1,HRules).		

%-------------------------------------------------------------------%

atomList_termList(Alist,Acc,Tlist,House) :-
    [HA|TA] = Alist,
    (atomic(HA) ->
	 atom_to_term(HA,Term,_BA),
	 %writeln(Term),
	 %BA.'Houses' = House,
	    Term =.. [Func|Rest] ,
	    %writeln(Func),
	    [_|Rest2] = Rest,
	    RestF = [House|Rest2],
	    TermF =.. [Func|RestF],
	    append([TermF],Acc,Acc1),
	    %printlist(Acc1),
	    atomList_termList(TA,Acc1,Tlist,House);
     writeln('POSSIVEL PROBLEMA'),
     atomList_termList(TA,Acc,Tlist,House)).

atomList_termList(_,R,R,_House).

listCall([]).

listCall(Tlist) :-
    [H|T] = Tlist,
    %writeln("\n\n\nCalling term"),
    %writeln(H),
    call(H),
    %writeln("End call\n\n"),
    listCall(T).

printlist([]).
    
printlist([X|List]) :-
    write(X),write(' , '),nl,
    printlist(List).

%-------------------------------------------------------------------%
%This section generates rules from the C list, using the con(X,Y)   %
%created in parse_D                                                 %
%-------------------------------------------------------------------%
parse_C([H|T],Rlist,Pacc,Aacc) :-
    [OP|ARG] = H,
    (OP == > -> parse_right(ARG,Rule,Kind),
		append([Rule],Aacc,Aacc1),
		parse_C(T,Rlist,Pacc,Aacc1);
     true),

    (OP == < -> parse_left(ARG,Rule,Kind),
		append([Rule],Aacc,Aacc1),
		parse_C(T,Rlist,Pacc,Aacc1);
     true),
    
    (OP == = -> parse_equal(ARG,Rule,Kind),
		append([Rule],Pacc,Pacc1),
		parse_C(T,Rlist,Pacc1,Aacc);
     true),

    (OP == or -> parse_or(ARG,Rule,Kind),
		 append([Rule],Pacc,Pacc1),
     		 parse_C(T,Rlist,Pacc1,Aacc);
     true).
    %writeln(Rule).
    %writeln(Pacc),
%Parse_C(T,Rlist,Pacc1,Aacc1).
parse_C([],Rlist,Pacc,Aacc) :-  append(Pacc,Aacc,Rlist).

parse_or(A,ORRules,or) :-
    [H,T|_] = A,
    [_,N,P1] = H,
    [_,N,P2] = T,
    con(NT,N),
    con(P1T,P1),
    con(P2T,P2),
    atom_concat('or_of(Houses, [',NT,ORRules0),
    atom_concat(ORRules0,'-\'',ORRules1),
    atom_concat(ORRules1,N,ORRules2),
    atom_concat(ORRules2,'\' , ',ORRules3),
    atom_concat(ORRules3,P1T,ORRules4),
    atom_concat(ORRules4,'-\'',ORRules5),
    atom_concat(ORRules5,P1,ORRules6),
    atom_concat(ORRules6,'\'], [',ORRules7),
    atom_concat(ORRules7,NT,ORRules8),
    atom_concat(ORRules8,'-\'',ORRules9),
    atom_concat(ORRules9,N,ORRules10),
    atom_concat(ORRules10,'\' , ',ORRules11),
    atom_concat(ORRules11,P2T,ORRules12),
    atom_concat(ORRules12,'-\'',ORRules13),
    atom_concat(ORRules13,P2,ORRules14),
    atom_concat(ORRules14,'\'])',ORRules).
    %writeln(ORRules).

parse_left(A,LRules,lf) :-
    [H|TT] = A,
    [T|_] = TT,
    parse_left_arg(H,RH),
    parse_left_arg(T,RT),
    con(RHC,RH),
    con(RTC,RT),
    atom_concat('two_of(Houses, left_of, [[',RHC,LRules0),
    atom_concat(LRules0,'-\'',LRules1), 
    atom_concat(LRules1,RH,LRules2),
    atom_concat(LRules2,'\'],[',LRules3),
    atom_concat(LRules3,RTC,LRules4),
    atom_concat(LRules4,'-\'',LRules5),
    atom_concat(LRules5,RT,LRules6),
    atom_concat(LRules6,'\']])',LRules).
    %writeln(LRules).

parse_left_arg(X,R) :-
    atomic(X),
    %string(X),
    R = X.

parse_right(A,RRules,rt) :-
    [H|TT] = A,
    [T|_] = TT,
    parse_right_arg(H,RH),
    parse_right_arg(T,RT),
    con(RHC,RH),
    con(RTC,RT),
    atom_concat('two_of(Houses, right_of, [[',RHC,RRules0),
    atom_concat(RRules0,'-\'',RRules1), 
    atom_concat(RRules1,RH,RRules2),
    atom_concat(RRules2,'\'],[',RRules3),
    atom_concat(RRules3,RTC,RRules4),
    atom_concat(RRules4,'-\'',RRules5),
    atom_concat(RRules5,RT,RRules6),
    atom_concat(RRules6,'\']])',RRules).
    %writeln(RRules).

parse_right_arg(X,R) :-
    atomic(X),
    R = X.

parse_equal(A,Rule,Kind) :-
    [H|TT] = A,
    [T|_] = TT,
    parse_equal_arg(H,RH1,RH2,OP1),
    %writeln(H),writeln(T),
    parse_equal_arg(T,RT1,_RT2,OP2),
    (OP1 == 'abs-' -> RH=RH1, RT=RH2;
     (OP1 == 'abs+' -> RH=RH1, RT=RT1;
      RH=RH1, RT=RT1)),
    con(RHC,RH),
    con(RTC,RT),
    Kind = OP1,
    atom_concat(OP1,OP2,OP),
    %writeln(OP),
    (OP == == ->
	 atom_concat('one_of(Houses, [',RHC,Rule0),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\' , ',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\'])',Rule); true),
	 %writeln(Rule); true),
    (OP == -= ->
	 atom_concat('two_of(Houses, r_right_of, [[',RHC,Rule0),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\'] , [',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\']])',Rule); true),
    (OP == '-2=' ->
	 atom_concat('two_of(Houses, rr_right_of, [[',RHC,Rule0),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\'] , [',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\']])',Rule); true),
    (OP == '-3=' ->
	 atom_concat('two_of(Houses, rrr_right_of, [[',RHC,Rule0),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\'] , [',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\']])',Rule); true),
    (OP == =- ->
	 atom_concat('two_of(Houses, r_left_of, [[',RHC,Rule0),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\'] , [',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\']])',Rule); true),

    (OP == += ->
	 atom_concat('two_of(Houses, r_left_of, [[',RHC,Rule0),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\'] , [',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\']])',Rule); true),
     (OP == '+2=' ->
	 atom_concat('two_of(Houses, rr_left_of, [[',RHC,Rule0),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\'] , [',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\']])',Rule); true),
     (OP == '+3=' ->
	 atom_concat('two_of(Houses, rrr_left_of, [[',RHC,Rule0),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\'] , [',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\']])',Rule); true),
    (OP == '=-2' ->
	 atom_concat('two_of(Houses, rr_left_of, [[',RHC,Rule0),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\'] , [',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\']])',Rule); true),
    (OP == '=-3' ->
	 atom_concat('two_of(Houses, rrr_left_of, [[',RHC,Rule0),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\'] , [',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\']])',Rule); true),

    (OP == =+ ->
	 atom_concat('two_of(Houses, r_right_of, [[',RHC,Rule0),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\'] , [',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\']])',Rule); true),

    (OP == '=+2' ->
	 atom_concat('two_of(Houses, rr_right_of, [[',RHC,Rule0),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\'] , [',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\']])',Rule); true),

    (OP == '=+3' ->
	 atom_concat('two_of(Houses, rrr_right_of, [[',RHC,Rule0),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\'] , [',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\']])',Rule); true),
    (OP1 == 'abs-' ->
	 (RT1 == 1 ->
	      atom_concat('two_of(Houses, next_to, [[',RHC,Rule0);
	  (RT1 == 2  ->
	       atom_concat('two_of(Houses, nnext_to, [[',RHC,Rule0);
	   atom_concat('two_of(Houses, nnnext_to, [[',RHC,Rule0))),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\'] , [',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\']])',Rule); true),
    (OP1 == 'abs+'  ->     
	 atom_concat('two_of(Houses, r_left_of, [[',RHC,Rule0),
	 atom_concat(Rule0,'-\'',Rule1),
	 atom_concat(Rule1,RH,Rule2),
	 atom_concat(Rule2,'\'] , [',Rule3),
	 atom_concat(Rule3,RTC,Rule4),
	 atom_concat(Rule4,'-\'',Rule5),
	 atom_concat(Rule5,RT,Rule6),
	 atom_concat(Rule6,'\']])',Rule); true).

parse_equal_arg(X,R1,R2,OP) :-
    \+atomic(X),
    [OPT|T] = X,
    [F1|F2l] = T,
    (OPT==abs -> [OPT2|I1] = F1,
		 [R1,R2|_] = I1,
		 atom_concat(OPT,OPT2,OP);
     [F2] = F2l,
     (F2 \= 1 ->
	  atom_concat(OPT,F2,OP);OP=OPT),
     R1=F1,
     R2=F2).

parse_equal_arg(X,R1,R2,OP) :-
    OP = =,
    atomic(X),
    %string(X),
    R1 = X,
    R2 = X.
%----------------------------------------------------------------------%
%Admito que esse bando de atom_concat e bem deselagante, tentar refazer%
%----------------------------------------------------------------------%


r_left_of(A,B,C):- append(_,[A,B|_],C).

r_right_of(A,B,C):- r_left_of(B,A,C).

rr_left_of(A,B,C):- append(_,[A,_,B|_],C).

rr_right_of(A,B,C):- rr_left_of(B,A,C).

rrr_left_of(A,B,C):- append(_,[A,_,_,B|_],C).

rrr_right_of(A,B,C):- rrr_left_of(B,A,C).

left_of(L,R,[L|T]) :-
    member(R,T).
left_of(L,R,[_|T]) :-
    left_of(L,R,T).

right_of(R,L,A):-
    left_of(L,R,A).

next_to(A,B,C):- r_left_of(A,B,C).
next_to(A,B,C):- r_left_of(B,A,C).

nnext_to(A,B,C):- rr_left_of(A,B,C).
nnext_to(A,B,C):- rr_left_of(B,A,C).

nnnext_to(A,B,C):- rrr_left_of(A,B,C).
nnnext_to(A,B,C):- rrr_left_of(B,A,C).

%Para garantir maior desempeenho, primeiro as select, depois
%as r_*, depois as next_to e só depois as left, right e between
%90% do ganho com 10% do esforco separando apenas as left e right

gera_houses(X,N) :-
    N \= 1,
    append(X,[_],X1),
    N1 is N-1,
    gera_houses(X1,N1).
gera_houses(_X,_).

attrs(H,[N-V|R]):- memberchk( N-X, H), X=V,  % unique attribute names
                   (R=[] -> true ; attrs(H,R)).
one_of(HS,AS)  :- member(H,HS), attrs(H,AS).
    
or_of(HS, AS1, AS2) :-
    (one_of(HS, AS1); one_of(HS, AS2)).
two_of(HS,G,AS):- call(G,H1,H2,HS), maplist(attrs,[H1,H2],AS).


%Meidanis sugeriu usar call para colocar essas regras como uma!






problemas(
	[
	    p1,
	    p2,
	    p3,
	    escola,
	    piscina,
	    compras,
	    presentes,
	    corruptos,
	    cinema2,
	    advocacia,
	    banco,
	    piscina2,
	    nadadores2,
	    talentos,
	    maratona,
	    navios,
	    nadadores,
	    estacionamento,
	    aeroporto,
	    escola2,
	    zoologico,
	    mecanico,
	    enem,
	    ingles,
	    kart,
	    biblioteca,
	    compras2,
	    reveillon,
	    aniversario,
	    judo,
	    nacoes,
	    futebol,
	    negocios,
	    pizza,
	    pais,
	    cientistas,
	    boliche,
	    farmacia,
	    secreto,
	    mulher,
	    casamento,
	    bicicleta,
	    danca,
	    antiguidades,
	    ilha,
	    criancas,
	    halloween,
	    astronomos,
	    mercado,
	    laboratorio,
	    estacao,
	    bercario,
	    gasolina,
	    copa,
	    piquenique,
	    prato,
	    musica,
	    futuro,
	    veterinario,
	    dentes,
	    morango,
	    lavanderia,
	    gestante,
	    onibus,
	    corruptos2,
	    ceia,
	    churrasco,
	    hidrica,
	    carnaval,
	    praia,
	    manifestacao,
	    fotografico,
	    italiano,
	    construcao,
	    bebe,
	    self,
	    avioes,
	    cinema,
	    academia,
	    pescador,
	    surf,
	    espera,
	    moda,
	    bandas,
	    maes,
	    pet,
	    aquario,
	    anonovo,
	    pascoa,
	    pedagio,
	    skate,
	    happy,
	    trabalho,
	    beleza,
	    porto2,
	    avioes2,
	    namorados,
	    ferias,
	    poesia,
	    antigos,
	    junina]).

problema(pais,
	[[domain(nome),adriana,carolina,daniela,mariana,patricia],[domain(bolsa),amarela,azul,branca,verde,vermelha],[domain(gastar),'50r','75r','100r','200r','300r'],[domain(pai),andre,carlos,fabio,joao,paulo],[domain(presente),calca_jeans,camisa,carteira,gravata,perfume],[domain(time),corinthians,palmeiras,portuguesa,santos,sao_paulo]],
	[[=,corinthians,4],[=,carlos,2],[or,[=,'50r',1],[=,'50r',5]],[=,[+,verde,1],carteira],[=,paulo,santos],[or,[=,'300r',1],[=,'300r',5]],[=,[abs,[-,carolina,palmeiras]],1],[=,joao,palmeiras],[>,portuguesa,branca],[<,portuguesa,palmeiras],[=,[+,'200r',1],'300r'],[=,fabio,4],[>,vermelha,andre],[<,vermelha,amarela],[=,[abs,[-,calca_jeans,perfume]],1],[=,daniela,gravata],[=,azul,2],[>,'100r',camisa],[<,'100r','200r'],[<,adriana,santos],[=,verde,'100r'],[<,patricia,perfume],[=,portuguesa,'75r'],[or,[=,adriana,'100r'],[=,adriana,'75r']],[=,[-,patricia,1],mariana]]).
problema(p1,
	 [[domain(cor),azul,vermelha,preta],[domain(nacionalidade),alemao,espanhol,italiano]],
	 [[=,espanhol,[+,vermelha,1]],[=,alemao,azul],[=,italiano,2]]).
problema(p2,
	 [[domain(cor),azul,branca,preta],[domain(nacionalidade),alemao,brasileiro,espanhol],[domain(animal),borboleta,cachorro,cavalo],[domain(esporte),futebol,tenis,sinuca]],
	 [[or,[=,brasileiro,1],[=,brasileiro,3]],[=,cachorro,futebol],[=,[+,tenis,2],preta],[=,cavalo,[-,borboleta,1]],[=,cachorro,[+,branca,1]],[=,espanhol,3]]).
problema(p3,
	 [[domain(cor),amarela,branca,preta,vermelha],[domain(nacionalidade),alemao,espanhol,frances,grego],[domain(animal),borboleta,cachorro,cavalo,tartaruga],[domain(esporte),basquete,futebol,tenis,sinuca]],
	 [[=,[abs,[-,basquete,tenis]],3],[=,grego,[+,futebol,2]],[=,2,amarela],[=,cavalo,[-,preta,2]],[=,[+,alemao,1],tartaruga],[=,cavalo,[-,borboleta,3]],[>,basquete,sinuca],[=,futebol,[-,vermelha,2]],[=,espanhol,1]]).
problema(escola,
	 [[domain(nome),ana,jessica,joana,pati,renata],[domain(mochila),amarela,azul,preta,verde,vermelha],[domain(materia),artes,biologia,historia,matematica,portugues],[domain(animal),cachorros,cavalos,gatos,hamsters,passaros],[domain(lugar),fernando_de_noronha,florianopolis,recife,rio_de_janeiro,salvador],[domain(suco),abacaxi,laranja,limao,maracuja,morango]],
	 [[=,joana,abacaxi],[=,hamsters,artes],[=,ana,limao],[<,jessica,renata],[=,pati,1],[>,artes,biologia],[>,artes,historia],[>,artes,matematica],[>,artes,portugues],[=,laranja,cavalos],[=,limao,3],[=,jessica,verde],[=,[+,florianopolis,1],3],[=,recife,amarela],[=,[abs,[-,abacaxi,fernando_de_noronha]],1],[=,vermelha,fernando_de_noronha],[=,amarela,1],[=,azul,cachorros],[=,[abs,[-,biologia,hamsters]],1],[<,historia,matematica],[=,[abs,[-,laranja,maracuja]],1],[=,preta,rio_de_janeiro],[=,morango,passaros],[=,[abs,[-,biologia,portugues]],1],[=,jessica,salvador]]).
problema(piscina,
	 [[domain(maio),amarelo,azul,branco,verde],[domain(nome),ana,bruna,raquel,vivian],[domain(idade),i8,i9,i10,i11],[domain(suco),laranja,limao,maracuja,morango],[domain(protetor),fps40,fps45,fps50,fps55],[domain(animal),cachorro,gato,passaro,peixe]],
	 [[=,3,cachorro],[or,[=,peixe,1],[=,peixe,4]],[=,gato,1],[=,ana,fps50],[=,2,fps55],[=,[abs,[-,fps40,i8]],1],[=,morango,4],[=,[abs,[-,maracuja,passaro]],1],[=,[abs,[-,limao,maracuja]],1],[or,[=,laranja,1],[=,laranja,4]],[<,azul,i9],[=,i8,4],[or,[=,i11,1],[=,i11,4]],[=,vivian,passaro],[=,raquel,1],[=,verde,4],[or,[=,branco,1],[=,branco,4]]]).
problema(compras,
	 [[domain(blusa),amarela,azul,branca,verde,vermelha],[domain(nome),aline,carol,fernanda,juliana,natalia],[domain(esqueceu),amaciante,frutas,leite,pao,presunto],[domain(pagamento),cheque,credito,debito,dinheiro,vale],[domain(veio_com),filho,irma,mae,marido,namorado]],
	 [[=,[abs,[+,pao,1]],irma],[<,azul,filho],[=,marido,vale],[=,namorado,1],[=,credito,4],[=,dinheiro,5],[=,[abs,[+,marido,1]],pao],[=,cheque,1],[=,credito,presunto],[=,leite,dinheiro],[or,[=,amaciante,1],[=,amaciante,5]],[=,fernanda,branca],[<,verde,azul],[=,aline,marido],[=,[-,carol,1],marido],[=,natalia,5],[=,[abs,[+,juliana,1]],amarela],[=,branca,presunto],[=,vermelha,filho]]).
problema(presentes,
	 [[domain(cor),amarela,azul,branca,verde,vermelha],[domain(nome),alex,cristian,eduardo,mario,pedro],[domain(idade),'6_anos','7_anos','8_anos','9_anos','10_anos'],[domain(presente),bicicleta,bola,computador,skate,video_game],[domain(suco),abacaxi,laranja,limao,maracuja,morango],[domain(profissao),astronauta,bombeiro,medico,policial,professor]],
	 [[or,[=,bombeiro,1],[=,bombeiro,5]],[=,maracuja,medico],[=,professor,3],[or,[=,policial,1],[=,policial,5]],[=,bombeiro,5],[or,[=,laranja,1],[=,laranja,5]],[=,[+,limao,1],morango],[=,[-,medico,1],abacaxi],[=,cristian,limao],[=,[abs,[-,'8_anos',computador]],1],[=,[+,skate,1],alex],[=,branca,video_game],[=,bola,1],[=,[abs,[-,bicicleta,video_game]],1],[=,'6_anos',5],[>,'10_anos','7_anos'],[<,'10_anos','9_anos'],[=,'9_anos',professor],[=,pedro,3],[=,[abs,[+,pedro,1]],eduardo],[=,eduardo,verde],[=,[+,branca,1],maracuja],[=,[+,azul,1],video_game],[=,amarela,1]]).
problema(corruptos,
	 [[domain(gravata),amarela,azul,branca,verde,vermelha],[domain(cargo),deputado,governador,ministro,prefeito,vereador],[domain(estado),goias,maranhao,para,parana,sao_paulo],[domain(acusacao),desvio,extorsao,nepotismo,propina,superfaturamento],[domain(fortuna),'30_m','35_m','40_m','45_m','50_m'],[domain(paraiso),chipre,ilhas_cayman,suica,tonga,uruguai]],
	 [[=,chipre,4],[>,ilhas_cayman,branca],[<,ilhas_cayman,chipre],[=,ilhas_cayman,3],[=,[+,branca,1],tonga],[=,[abs,[-,chipre,uruguai]],1],[=,goias,'50_m'],[>,'45_m',deputado],[<,'45_m','30_m'],[=,[-,'40_m',1],suica],[or,[=,nepotismo,1],[=,nepotismo,5]],[=,propina,4],[>,extorsao,deputado],[<,extorsao,desvio],[=,nepotismo,5],[>,goias,suica],[<,goias,'30_m'],[=,parana,3],[>,maranhao,sao_paulo],[<,maranhao,azul],[>,prefeito,superfaturamento],[<,prefeito,governador],[>,vereador,sao_paulo],[<,vereador,prefeito],[=,ministro,tonga],[=,[-,azul,1],verde],[<,verde,vermelha],[=,verde,ilhas_cayman]]).
problema(cinema2,
	 [[domain(nome),bruna,carol,fernanda,marcela,sabrina],[domain(namorado),alex,daniel,henrique,luis,yuri],[domain(idade),'21_anos','22_anos','23_anos','24_anos','25_anos'],[domain(cor),amarelo,azul,branco,verde,vermelho],[domain(filme),acao,comedia,drama,ficcao,romance]],
	 [[=,[abs,[-,vermelho,comedia]],1],[=,'24_anos',ficcao],[=,acao,2],[=,drama,1],[=,azul,5],[=,'23_anos',romance],[=,[abs,[-,verde,henrique]],1],[=,'22_anos',amarelo],[=,alex,romance],[=,[-,'23_anos',1],amarelo],[=,'21_anos',daniel],[=,ficcao,5],[=,'23_anos',3],[=,henrique,5],[=,[abs,[-,luis,acao]],1],[=,[abs,[-,sabrina,'24_anos']],1],[=,[abs,[-,carol,'22_anos']],1],[=,fernanda,henrique],[or,[=,bruna,1],[=,bruna,5]]]).
problema(advocacia,
	 [[domain(gravata),amarela,azul,branca,verde,vermelha],[domain(area),civil,comercial,consumidor,imobiliaria,trabalhista],[domain(nome),dr_alberto,dr_carlos,dr_luis,dr_otavio,dr_ulysses],[domain(bebida),caipirinha,cosmopolitan,margarita,martini,mojito],[domain(idade),'30_anos','34_anos','37_anos','40_anos','44_anos'],[domain(carro),crossover,hatch,pickup,sedan,suv]],
	 [[>,suv,crossover],[<,suv,sedan],[=,dr_luis,suv],[<,amarela,hatch],[=,crossover,1],[=,'37_anos',4],[>,'34_anos',dr_otavio],[<,'34_anos',civil],[=,'30_anos',2],[=,[-,mojito,1],'40_anos'],[=,[+,'34_anos',1],cosmopolitan],[=,caipirinha,5],[=,martini,3],[=,suv,4],[or,[=,dr_carlos,1],[=,dr_carlos,5]],[>,dr_ulysses,amarela],[>,amarela,dr_otavio],[<,amarela,'34_anos'],[=,civil,4],[=,[abs,[-,trabalhista,comercial]],1],[>,trabalhista,crossover],[<,trabalhista,comercial],[=,[+,imobiliaria,1],trabalhista],[=,[-,branca,1],dr_luis],[=,vermelha,4],[=,azul,3]]).
problema(banco,
	 [[domain(camisa),amarela,azul,branca,verde,vermelha],[domain(conta),agua,aluguel,celular,luz,telefone],[domain(nome),augusto,douglas,jose,ronaldo,samuel],[domain(profissao),bombeiro,fotografo,massagista,pesquisador,veterinario],[domain(animal),cachorro,gato,passaro,peixe,tartaruga],[domain(esporte),basquete,futebol,sinuca,natacao,volei]],
	 [[=,sinuca,4],[=,futebol,5],[=,basquete,2],[=,volei,vermelha],[=,[+,peixe,1],natacao],[=,branca,tartaruga],[=,[-,bombeiro,1],samuel],[=,veterinario,5],[=,pesquisador,3],[=,douglas,fotografo],[=,jose,5],[=,[+,pesquisador,1],augusto],[=,douglas,cachorro],[=,telefone,5],[=,agua,branca],[=,luz,3],[=,ronaldo,celular],[=,verde,passaro],[or,[=,azul,1],[=,azul,5]],[=,[-,gato,1],sinuca],[=,[abs,[-,passaro,sinuca]],1]]).
problema(nadadores2,
	 [[domain(touca),amarela,azul,branca,verde],[domain(pais),argentina,brasil,eua,franca],[domain(especialidade),borboleta,costas,crawl,peito],[domain(medalhas),'2medalhas','3medalhas','5medalhas','8medalhas'],[domain(peso),'70kg','75kg','80kg','84kg'],[domain(idade),'20anos','22anos','23anos','24anos']],
	 [[or,[=,'22anos',1],[=,'22anos',4]],[=,'23anos',2],[=,[abs,[-,'24anos','75kg']],1],[=,'22anos','8medalhas'],[=,'84kg',2],[=,[abs,[-,'70kg',amarela]],1],[=,'5medalhas',3],[=,'2medalhas',1],[=,crawl,4],[=,'24anos',costas],[=,[abs,[-,peito,eua]],1],[=,eua,3],[=,argentina,2],[=,branca,3],[=,azul,2],[=,[abs,[-,argentina,franca]],1]]).
problema(talentos,
	 [[domain(camisa),amarela,azul,branca,verde,vermelha],[domain(nome),alexander,heitor,junior,michel,rodolfo],[domain(talento),canto,danca,imitacao,malabarismo,musica],[domain(idade),'22anos','24anos','29anos','32anos','35anos'],[domain(fruta),abacaxi,banana,maca,morango,uva],[domain(esporte),basquete,boliche,corrida,futebol,natacao]],
	 [[>,basquete,'22anos'],[<,basquete,corrida],[=,basquete,4],[=,futebol,2],[=,[+,michel,1],boliche],[=,uva,corrida],[=,[+,michel,1],maca],[=,[abs,[-,morango,abacaxi]],1],[=,[abs,[-,branca,morango]],1],[=,[abs,[-,'35anos',amarela]],1],[<,vermelha,'24anos'],[=,'32anos',2],[=,[+,'22anos',1],michel],[=,[-,canto,1],junior],[<,malabarismo,canto],[>,futebol,imitacao],[or,[=,canto,1],[=,canto,5]],[>,rodolfo,vermelha],[<,alexander,'32anos'],[=,[+,'22anos',1],michel],[=,[-,canto,1],junior],[=,[+,malabarismo,1],canto],[=,[-,futebol,1],imitacao],[=,[+,alexander,1],'32anos'],[=,junior,verde],[=,[-,abacaxi,1],natacao],[=,musica,vermelha]]).
problema(maratona,
	 [[domain(camiseta),amarelo,azul,branco,verde,vermelha],[domain(nome),edgar,lucio,moacir,paulo,tiago],[domain(idade),'25anos','28anos','34anos','39anos','41anos'],[domain(numero),'455i','612i','708i','899i','963i'],[domain(estado),bahia,minas_gerais,parana,rio_de_janeiro,sao_paulo],[domain(hobby),cantar,dancar,desenhar,pintar,ler]],
	 [[>,amarelo,bahia],[<,amarelo,lucio],[=,pintar,3],[or,[=,'28anos',1],[=,'28anos',5]],[=,[+,lucio,1],'455i'],[=,'899i',3],[=,sao_paulo,5],[=,[-,cantar,1],'34anos'],[=,moacir,verde],[=,amarelo,2],[=,[-,'41anos',1],'899i'],[=,[abs,[-,rio_de_janeiro,'455i']],1],[=,tiago,pintar],[>,amarelo,edgar],[<,amarelo,'28anos'],[=,[abs,[-,pintar,'963i']],1],[<,vermelha,'612i'],[=,ler,1],[=,vermelha,'39anos'],[=,'455i',5],[=,minas_gerais,3],[>,dancar,pintar],[<,dancar,desenhar],[=,branco,4]]).
problema(navios,
	 [[domain(nacionalidade),brasileiro,espanhol,frances,grego,ingles],[domain(saida),'5h','6h','7h','8h','9h'],[domain(carregamento),arroz,cacau,cafe,cha,milho],[domain(chamine),azul,branca,verde,vermelha,preta],[domain(destino),hamburgo,macau,manila,santos,rotterdam]],
	 [[=,grego,'6h'],[=,grego,cafe],[=,preta,3],[=,ingles,'9h'],[=,frances,azul],[=,[+,frances,1],cafe],[=,[-,macau,1],cacau],[=,brasileiro,manila],[=,[abs,[-,arroz,verde]],1],[=,santos,'5h'],[=,espanhol,'7h'],[=,[-,espanhol,1],macau],[=,vermelha,hamburgo],[=,[abs,[-,branca,'7h']],1],[or,[=,milho,1],[=,milho,5]],[=,preta,'8h'],[=,[abs,[-,milho,arroz]],1],[=,hamburgo,'6h']]).
problema(nadadores,
	 [[domain(touca),amarela,azul,branca,verde],[domain(pais),brasil,china,eua,russia],[domain(especialidade),borboleta,costas,crawl,peito],[domain(medalhas),'2medalhas','3medalhas','5medalhas','8medalhas'],[domain(suco),abacaxi,laranja,limao,maracuja],[domain(idade),'19anos','20anos','21anos','24anos']],
	 [[=,'24anos','8medalhas'],[<,azul,'19anos'],[or,[=,'20anos',1],[=,'20anos',4]],[=,[+,limao,1],crawl],[>,maracuja,azul],[=,laranja,2],[=,'24anos',3],[=,'3medalhas',2],[=,[+,'5medalhas',1],laranja],[=,peito,2],[=,[-,costas,1],'3medalhas'],[=,brasil,'8medalhas'],[=,[+,china,1],brasil],[=,eua,'5medalhas'],[=,branca,4],[=,[abs,[-,borboleta,azul]],1],[=,eua,verde]]).
problema(estacionamento,
	 [[domain(dono),alexandre,felipe,gilmar,guilherme,rogerio],[domain(cor),amarela,azul,branca,verde,vermelha],[domain(montadora),alema,chinesa,francesa,italiana,japonesa],[domain(placa),aaa1111,bbb2222,ccc3333,ddd4444,ee5555],[domain(ano),'2007a','2008a','2009a','2010a','2011a'],[domain(tipo),crossover,hatch,pickup,sedan,suv]],
	 [[=,'2009a',pickup],[=,alexandre,crossover],[=,suv,2],[=,[abs,[-,sedan,pickup]],1],[=,pickup,5],[=,'2010a',3],[=,[abs,[-,'2008a',azul]],1],[=,ccc3333,'2007a'],[=,sedan,ee5555],[=,[abs,[-,ddd4444,guilherme]],1],[>,alema,felipe],[=,francesa,4],[=,[-,branca,1],japonesa],[=,[+,italiana,1],crossover],[=,crossover,amarela],[<,chinesa,branca],[=,vermelha,2],[or,[=,rogerio,1],[=,rogerio,5]],[=,italiana,'2007a'],[=,[-,gilmar,1],branca],[>,alexandre,ddd4444],[<,alexandre,aaa1111]]).
problema(aeroporto,
	 [[domain(mala),amarela,azul,branca,verde,vermelha],[domain(gentilico),baiano,gaucho,fluminense,mineiro,paulista],[domain(destino),australia,franca,italia,inglaterra,japao],[domain(horario),'8h','9h','10h','11h','12h'],[domain(finalidade),congresso,estudos,intercambio,negocios,turismo],[domain(esqueceu),agenda,celular,notebook,oculos,terno]],
	 [[=,terno,5],[=,inglaterra,agenda],[<,branca,notebook],[=,[+,turismo,1],celular],[or,[=,congresso,1],[=,congresso,5]],[>,estudos,verde],[=,[-,negocios,1],intercambio],[=,'8h',4],[=,[-,baiano,1],celular],[=,paulista,franca],[or,[=,japao,1],[=,japao,5]],[or,[=,turismo,1],[=,turismo,5]],[=,[abs,[-,'9h',gaucho]],1],[=,[abs,[-,verde,mineiro]],1],[=,japao,'9h'],[=,[abs,[-,amarela,notebook]],1],[=,branca,2],[>,'11h',verde],[<,branca,'10h'],[or,[=,'9h',1],[=,'9h',5]],[=,[abs,[-,australia,'11h']],1],[=,vermelha,1]]).
problema(escola2,
	 [[domain(nome),aline,beatriz,isabelle,juliana,patricia],[domain(mochila),amarela,azul,branca,verde,vermelha],[domain(materia),artes,biologia,historia,matematica,portugues],[domain(animal),cachorros,cavalos,gatos,hamsters,passaros],[domain(lugar),florianopolis,manaus,recife,rio_de_janeiro,salvador],[domain(suco),abacaxi,laranja,limao,maracuja,morango]],
	 [[or,[=,laranja,1],[=,laranja,5]],[>,maracuja,cavalos],[<,maracuja,morango],[=,[abs,[-,limao,vermelha]],1],[=,[abs,[-,isabelle,laranja]],1],[=,hamsters,rio_de_janeiro],[=,[+,aline,1],manaus],[>,florianopolis,aline],[or,[=,recife,1],[=,recife,5]],[=,biologia,rio_de_janeiro],[=,[abs,[-,gatos,branca]],1],[or,[=,passaros,1],[=,passaros,5]],[=,[abs,[-,hamsters,cachorros]],1],[=,[abs,[-,branca,passaros]],1],[=,cachorros,5],[=,[abs,[-,matematica,cavalos]],1],[=,verde,historia],[=,aline,portugues],[=,[+,matematica,1],hamsters],[=,[+,vermelha,1],biologia],[=,azul,biologia],[>,aline,recife],[<,aline,verde],[=,beatriz,3],[=,patricia,1]]).
problema(zoologico,
	 [[domain(mochila),amarela,azul,branca,verde,vermelha],[domain(nome),ana,jessica,joana,pati,renata],[domain(suco),abacaxi,laranja,limao,maracuja,morango],[domain(lanche),banana,chocolate,maca,salgadinho,sanduiche],[domain(animal),arara,girafa,elefante,macaco,leao],[domain(materia),biologia,geografia,historia,portugues,matematica]],
	 [[=,biologia,4],[<,amarela,portugues],[>,historia,joana],[<,historia,geografia],[=,[+,pati,1],girafa],[=,[abs,[-,arara,leao]],1],[<,vermelha,leao],[=,macaco,branca],[=,maca,4],[=,geografia,salgadinho],[=,chocolate,2],[=,sanduiche,portugues],[or,[=,morango,1],[=,morango,5]],[>,limao,elefante],[<,limao,maracuja],[=,amarela,limao],[=,historia,laranja],[or,[=,abacaxi,1],[=,abacaxi,5]],[=,[-,morango,1],maracuja],[>,renata,banana],[<,renata,ana],[=,jessica,2],[=,pati,limao],[=,[abs,[-,geografia,azul]],1],[=,[-,vermelha,1],verde]]).
problema(mecanico,
	 [[domain(cor),amarela,azul,branca,verde,vermelha],[domain(montadora),alema,chinesa,francesa,italiana,japonesa],[domain(dono),adailton,francisco,george,nilton,marcos],[domain(tipo),crossover,hatch,pickup,sedan,suv],[domain(problema),bateria,cambio,embreagem,freio,motor],[domain(ano),'2007a','2008a','2009a','2010a','2011a']],
	 [[or,[=,'2008a',1],[=,'2008a',5]],[>,'2010a',hatch],[<,'2010a','2011a'],[=,[abs,[-,japonesa,'2009a']],1],[=,[abs,[-,branca,'2008a']],1],[=,sedan,freio],[=,[-,cambio,1],amarela],[<,amarela,bateria],[or,[=,embreagem,1],[=,embreagem,5]],[=,francisco,freio],[=,[abs,[-,pickup,francesa]],1],[or,[=,suv,1],[=,suv,5]],[=,[abs,[-,sedan,crossover]],1],[=,[abs,[-,suv,francesa]],1],[=,crossover,5],[=,[abs,[-,hatch,nilton]],1],[=,george,italiana],[=,marcos,amarela],[=,[+,nilton,1],sedan],[=,[+,japonesa,1],francisco],[=,francisco,chinesa],[>,amarela,embreagem],[<,amarela,italiana],[=,azul,3],[=,vermelha,1]]).
problema(enem,
	 [[domain(mochila),amarela,azul,branca,verde,vermelha],[domain(nome),andre,bruno,luiz,rafael,tiago],[domain(universidade),unesp,unicamp,unifesp,ufscar,usp],[domain(curso),computacao,direito,economia,odontologia,medicina],[domain(idade),'17a','18a','19a','20a','21a'],[domain(bairro),bela_vista,bom_retiro,consolacao,higienopolis,liberdade]],
	 [[=,'21a',liberdade],[=,[abs,[-,bom_retiro,bela_vista]],1],[=,bela_vista,unifesp],[=,consolacao,5],[=,'17a',direito],[=,[abs,[-,'19a',economia]],1],[=,[+,unicamp,1],'18a'],[=,[abs,[-,liberdade,'17a']],1],[>,medicina,bruno],[<,medicina,branca],[=,computacao,'18a'],[=,'17a',ufscar],[=,bruno,bom_retiro],[=,[+,higienopolis,1],unesp],[or,[=,unicamp,1],[=,unicamp,5]],[<,branca,rafael],[=,luiz,branca],[>,vermelha,higienopolis],[<,vermelha,usp],[or,[=,andre,1],[=,andre,5]],[>,verde,unicamp],[<,verde,economia],[=,vermelha,2],[=,amarela,odontologia]]).
problema(ingles,
	 [[domain(caderno),amarelo,azul,branco,verde,vermelho],[domain(nome),alex,bruno,diego,gabriel,henrique],[domain(pais),australia,canada,estados_unidos,inglaterra,nova_zelandia],[domain(idioma),alemao,espanhol,frances,italiano,mandarim],[domain(idade),'19a','20a','21a','22a','23a'],[domain(curso),biologia,direito,engenharia_eletrica,historia,quimica]],
	 [[=,diego,direito],[=,engenharia_eletrica,3],[or,[=,biologia,1],[=,biologia,5]],[=,[+,historia,1],italiano],[=,[abs,[-,'23a','20a']],1],[=,[+,canada,1],'20a'],[=,henrique,'23a'],[=,[abs,[-,verde,'19a']],1],[>,amarelo,'21a'],[<,amarelo,verde],[=,alemao,4],[>,frances,bruno],[<,frances,diego],[=,mandarim,1],[<,verde,inglaterra],[=,nova_zelandia,4],[or,[=,australia,1],[=,australia,5]],[=,historia,mandarim],[>,henrique,'21a'],[<,henrique,alex],[=,[abs,[-,bruno,australia]],1],[=,'23a',vermelho],[=,gabriel,historia],[>,branco,azul]]).
problema(kart,
	 [[domain(capacete),amarelo,azul,branco,verde,vermelho],[domain(nome),bruno,emerson,felipe,fernando,rubens],[domain(idade),'22a','25a','27a','29a','31a'],[domain(idolo),a_prost,f_alonso,k_raikkonen,m_schumacher,n_piquet],[domain(carro),crossover,hatch,pickup,sedan,suv],[domain(equipe),ferrari,mclaren,rbr,renault,williams]],
	 [[>,ferrari,a_prost],[<,ferrari,williams],[>,rbr,mclaren],[<,rbr,ferrari],[=,sedan,ferrari],[=,mclaren,2],[=,rbr,crossover],[or,[=,pickup,1],[=,pickup,5]],[>,amarelo,suv],[<,amarelo,rbr],[=,pickup,k_raikkonen],[=,[+,f_alonso,1],williams],[=,[-,verde,1],n_piquet],[or,[=,a_prost,1],[=,a_prost,5]],[=,'22a',k_raikkonen],[>,'27a','29a'],[<,'27a','25a'],[>,felipe,'27a'],[=,suv,'29a'],[=,[abs,[-,emerson,'25a']],1],[=,'27a',n_piquet],[>,emerson,a_prost],[<,emerson,sedan],[>,bruno,rubens],[<,bruno,rbr],[=,f_alonso,'25a'],[=,[+,vermelho,1],'25a'],[=,[+,felipe,1],branco]]).
problema(biblioteca,
	 [[domain(mochila),amarela,azul,branca,verde,vermelha],[domain(nome),carlos,fernando,mario,pedro,victor],[domain(livro),'1984l',clube_da_luta,dia_do_curinga,laranja_mecanica,tubarao],[domain(autor),agatha_christie,clarice_lispector,frans_kafta,machado_de_assis,paulo_coelho],[domain(profissao),eletricista,garcom,musico,professor,vendedor],[domain(lugar),biblioteca,jardim,onibus,quarto,sala]],
	 [[=,jardim,4],[=,biblioteca,3],[=,quarto,5],[=,musico,onibus],[<,vermelha,professor],[>,garcom,azul],[=,eletricista,agatha_christie],[=,laranja_mecanica,clarice_lispector],[=,[+,machado_de_assis,1],professor],[=,carlos,vendedor],[=,[abs,[-,vendedor,sala]],1],[=,frans_kafta,1],[=,[abs,[-,mario,'1984l']],1],[<,azul,laranja_mecanica],[=,[+,sala,1],dia_do_curinga],[=,clube_da_luta,2],[=,victor,2],[=,fernando,jardim],[=,machado_de_assis,'1984l'],[=,[abs,[-,azul,fernando]],1],[=,[abs,[-,vermelha,quarto]],1],[or,[=,branca,1],[=,branca,5]],[=,eletricista,verde],[=,[-,branca,1],'1984l']]).
problema(compras2,
	 [[domain(blusa),amarela,azul,branca,verde,vermelha],[domain(nome),aline,carol,fernanda,juliana,natalia],[domain(esqueceu),amaciante,frutas,leite,pao,presunto],[domain(pagamento),cheque,credito,debito,dinheiro,vale],[domain(foicom),filho,irma,mae,marido,namorado],[domain(carro),crossover,hatch,pickup,sedan,suv]],
	 [[=,[+,amaciante,1],sedan],[=,[-,crossover,1],debito],[=,namorado,pickup],[=,[+,sedan,1],suv],[=,pao,mae],[=,[abs,[-,aline,filho]],1],[or,[=,marido,1],[=,marido,5]],[=,cheque,4],[or,[=,dinheiro,1],[=,dinheiro,5]],[=,[+,debito,1],vale],[=,juliana,mae],[=,presunto,debito],[=,[abs,[-,amarela,frutas]],1],[=,pao,suv],[=,fernanda,filho],[=,[-,carol,1],amaciante],[=,azul,4],[<,verde,vermelha],[=,[abs,[-,frutas,presunto]],1],[>,amarela,marido],[<,amarela,verde],[=,[abs,[-,dinheiro,sedan]],1]]).
problema(reveillon,
	 [[domain(vestido),amarelo,azul,branco,verde,vermelho],[domain(nome),bruna,camila,gabriela,marina,vanessa],[domain(bebida),agua,cerveja,champagne,suco,vinho],[domain(idade),'23a','24a','25a','26a','27a'],[domain(desejo),carro,casa,casar,viajar,emagrecer],[domain(profissao),arquiteta,dentista,designer,jornalista,psicologa]],
	 [[>,arquiteta,designer],[<,arquiteta,jornalista],[=,[+,dentista,1],arquiteta],[=,jornalista,5],[=,[+,'26a',1],designer],[=,[-,casar,1],carro],[=,casar,4],[=,[+,branco,1],casa],[=,[abs,[-,emagrecer,'24a']],1],[=,[-,'25a',1],'27a'],[>,'23a',gabriela],[<,'23a','27a'],[=,[+,vanessa,1],vinho],[=,agua,3],[=,[abs,[-,champagne,verde]],1],[>,amarelo,suco],[<,amarelo,vermelho],[<,azul,marina],[<,camila,casa],[=,'25a',verde],[=,[abs,[-,gabriela,azul]],1],[=,vermelho,'27a']]).
problema(aniversario,
	 [[domain(vestido),amarelo,azul,branco,verde,vermelho],[domain(nome),andrea,barbara,cristiane,monica,rafaela],[domain(presente),bolsa,dvd,livro,sandalia,vestido],[domain(idade),'13a','14a','15a','16a','17a'],[domain(suco),abacaxi,laranja,limao,maracuja,morango],[domain(animal),cachorros,cavalos,gatos,golfinhos,passaros]],
	 [[or,[=,golfinhos,1],[=,golfinhos,5]],[=,maracuja,cachorros],[=,[abs,[-,'13a',gatos]],1],[=,'14a',cavalos],[=,abacaxi,5],[=,limao,gatos],[=,[abs,[-,cachorros,abacaxi]],1],[=,[-,rafaela,1],morango],[=,[abs,[-,azul,gatos]],1],[or,[=,'16a',1],[=,'16a',5]],[<,azul,'17a'],[=,[abs,[-,'15a',limao]],1],[=,[+,'17a',1],'13a'],[=,[abs,[-,cavalos,rafaela]],1],[=,vestido,5],[=,branco,livro],[=,[-,dvd,1],branco],[=,[abs,[-,barbara,sandalia]],1],[or,[=,andrea,1],[=,andrea,5]],[=,monica,3],[=,[abs,[-,amarelo,gatos]],1],[=,[-,cristiane,1],monica],[<,vermelho,andrea]]).
problema(judo,
	 [[domain(faixa),amarela,azul,branca,verde,vermelha],[domain(nome),diego,emerson,felipe,milton,renato],[domain(idade),'23a','25a','28a','30a','32a'],[domain(peso),'75kg','79kg','81kg','84kg','87kg'],[domain(altura),'175cm','178cm','180cm','184cm','187cm'],[domain(estado),es,go,rj,rs,sp]],
	 [[=,[abs,[-,'79kg',es]],1],[=,sp,5],[=,[-,vermelha,1],go],[=,rj,branca],[=,'175cm',5],[=,[abs,[-,emerson,'187cm']],1],[=,'184cm',3],[or,[=,'178cm',1],[=,'178cm',5]],[or,[=,'81kg',1],[=,'81kg',5]],[<,vermelha,'84kg'],[=,[abs,[-,go,rs]],1],[=,azul,'87kg'],[=,[+,'75kg',1],'25a'],[=,[+,'30a',1],'175cm'],[=,'32a',sp],[=,[-,'28a',1],go],[=,milton,amarela],[=,renato,'79kg'],[=,felipe,2],[>,emerson,branca],[=,branca,1]]).
problema(nacoes,
	 [[domain(estande),amarelo,azul,branco,verde,vermelho],[domain(representante),cassio,gabriel,helena,lais,sandro],[domain(nacao),alemanha,espanha,italia,japao,portugal],[domain(idade),'7a','8a','9a','10a','11a'],[domain(professor),jucenir,marcio,marilena,rosa,rogerio]],
	 [[<,amarelo,jucenir],[=,amarelo,cassio],[=,sandro,5],[=,cassio,rogerio],[<,verde,rosa],[=,'10a',marcio],[=,[abs,[-,alemanha,'9a']],1],[>,'7a',branco],[=,[-,'10a',1],vermelho],[=,'8a',3],[=,[+,portugal,1],espanha],[=,[abs,[-,branco,portugal]],1],[=,[abs,[-,'9a',jucenir]],1],[<,japao,espanha],[=,branco,rosa],[=,[-,sandro,1],cassio],[=,[abs,[-,amarelo,gabriel]],1],[or,[=,helena,1],[=,helena,5]],[=,[-,japao,1],italia]]).
problema(futebol,
	 [[domain(chuteira),amarela,azul,branca,verde,vermelha],[domain(nome),edson,fernando,marcos,rogerio,ronaldo],[domain(idade),'21a','22a','25a','26a','28a'],[domain(posicao),atacante,lateral,meia,volante,zagueiro],[domain(estado),ceara,para,parana,rio,sao_paulo],[domain(titulos),'3t','5t','6t','8t','10t']],
	 [[<,amarela,'3t'],[=,volante,'5t'],[>,'10t',branca],[>,branca,'6t'],[<,branca,vermelha],[=,ronaldo,parana],[<,rogerio,ceara],[=,[abs,[-,sao_paulo,amarela]],1],[=,[-,atacante,1],rio],[=,rogerio,rio],[=,meia,2],[=,[+,lateral,1],branca],[=,'28a',5],[=,[abs,[-,'10t','26a']],1],[=,'22a',3],[=,meia,'25a'],[=,[abs,[-,edson,meia]],1],[=,[abs,[-,ceara,'8t']],1],[>,branca,'6t'],[<,branca,edson],[=,[-,rogerio,1],para],[=,[+,marcos,1],'25a'],[=,[-,ronaldo,1],'22a'],[=,[-,volante,1],azul]]).
