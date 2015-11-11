:- module(tpman,[solver/2]).

my_atom_number(X,Y) :-
    atom_codes(X,T),\+memberchk(95,T),
    atom_number(X,Y).

solver(File,Sol) :-
    %Definir File como input
    start(File,C,D),
    solver_DCS(D,C,S),
    Sol = S,
    %print_S(S,_,Sol),
    maplist(writeln, S),
    !.

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
    (my_atom_number(P1,PP1) -> RP1 = PP1; RP1 = P1),
    (my_atom_number(P2,PP2) -> RP2 = PP2; RP2 = P2),
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
    (my_atom_number(N2,NN2) ->
	 R = ['=',['abs',['+',N1,1]],NN2];
     R = ['=',['abs',['+',N1,1]],N2]).
    
read_neg_abs_rule(S,R) :-
    %writeln('Parsing negative abs'),
    [SH,SL] = S,
    split_atom(' ',SH,S1),
    split_atom(' ',SL,S2),
    [N1,_OP,N2|_] = S1,
    [_,X] = S2,
    my_atom_number(X,XX),
    (my_atom_number(N2,NN2) ->
	 R = ['=',['abs',['-',N1,NN2]],XX];
     R = ['=',['abs',['-',N1,N2]],XX]).
read_plus_equal_rule(S,R) :-
    %write('Parsing positive equal'),
    %writeln(S),
    ([N1,'+',X,_,N2] = S ->
	 my_atom_number(X,XX),
	 (my_atom_number(N1,NN1) ->
	      (my_atom_number(N2,NN2) ->
		   R = ['=',['+',NN1,XX],NN2];
	       R = ['=',['+',NN1,XX],N2]);
	  (my_atom_number(N2,NN2) ->
	       R = ['=',['+',N1,XX],NN2];
	   R = ['=',['+',N1,XX],N2]));
     [N1,_,N2,'+',X] = S,
     my_atom_number(X,XX),
     (my_atom_number(N2,NN2) ->
	  R = ['=',N1,['+',NN2,XX]];
      R = ['=',N1,['+',N2,XX]])).

read_minus_equal_rule(S,R) :-
    %write('Parsing negative equal'),
    %writeln(S),
    ([N1,'-',X,_,N2] = S ->
	 my_atom_number(X,XX),
	 (my_atom_number(N2,NN2) ->
	      R = ['=',['-',N1,XX],NN2];
	  R = ['=',['-',N1,XX],N2]);
     [N1,_,N2,'-',X] = S,
     my_atom_number(X,XX),
     (my_atom_number(N1,NN1) ->
	  (my_atom_number(N2,NN2) ->
	       R = ['=',NN1,['-',NN2,XX]];
       R = ['=',NN1,['-',N2,XX]]);
      (my_atom_number(N2,NN2) ->
	   R = ['=',N1,['-',NN2,XX]];
       R = ['=',N1,['-',N2,XX]]))).

read_real_equal_rule(S,R) :-
    %write('Parsing real equal'),
    %writeln(S),
    [N1,_,N2] = S,
    %my_atom_number(N2,NN2),
    (my_atom_number(N1,NN1) ->
	 (my_atom_number(N2,NN2) -> R = ['=',NN1,NN2];
	  R = ['=',NN1,N2]);
     (my_atom_number(N2,NN2) -> R = ['=',N1,NN2];
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

print_S([],R,R).
print_S(S,Acc,SS) :-
    [H|T] = S,
    c(Name,Estate) = H,
    atom_concat(Name,' = ', R1),
    atom_concat(R1,' ',R2),
    atom_concat(R2,Estate, R3),
    writeln(R3),
    append([R3],Acc,Acc1),
    %write(Name), write(' = '),writeln(Estate),
    print_S(T,Acc1,SS).
    
solver_DCS(D,C,S) :-
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
    my_atom_number(N,Num),
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


%Ficaram horriveis esses predicados abaixo, muito feios
%1 a direita e esquerda
r_left_of(A,B,C):- append(_,[A,B|_],C).

r_right_of(A,B,C):- r_left_of(B,A,C).

%2 a direita e esquerda
rr_left_of(A,B,C):- append(_,[A,_,B|_],C).

rr_right_of(A,B,C):- rr_left_of(B,A,C).

%3 a direita e esquerda
rrr_left_of(A,B,C):- append(_,[A,_,_,B|_],C).

rrr_right_of(A,B,C):- rrr_left_of(B,A,C).

%arbitrario a direita e esquerda
left_of(L,R,[L|T]) :-
    member(R,T).
left_of(L,R,[_|T]) :-
    left_of(L,R,T).

right_of(R,L,A):-
    left_of(L,R,A).

%Next de 1, 2 e 3
next_to(A,B,C):- r_left_of(A,B,C).
next_to(A,B,C):- r_left_of(B,A,C).

nnext_to(A,B,C):- rr_left_of(A,B,C).
nnext_to(A,B,C):- rr_left_of(B,A,C).

nnnext_to(A,B,C):- rrr_left_of(A,B,C).
nnnext_to(A,B,C):- rrr_left_of(B,A,C).

%Para garantir maior desempeenho, primeiro as select, depois
%as r_*, depois as next_to e só depois as left e right
%90% do ganho com 10% do esforco separando apenas as left e right

gera_houses(X,N) :-
    N \= 1,
    append(X,[_],X1),
    N1 is N-1,
    gera_houses(X1,N1).
gera_houses(_X,_).

attribs(H,[N-V|R]):- memberchk( N-X, H), X=V,
                   (R=[] -> true ; attribs(H,R)).
one_of(HS,AS)  :- member(H,HS), attribs(H,AS).
    
or_of(HS, AS1, AS2) :-
    (one_of(HS, AS1); one_of(HS, AS2)).
two_of(HS,G,AS):- call(G,H1,H2,HS), maplist(attribs,[H1,H2],AS).

%Meidanis sugeriu usar call para colocar essas regras como uma!
