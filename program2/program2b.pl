% For compiled prolog
%:- initialization(go).
%:- dynamic(known/3).

%consult('sets.pl').

%   Scanning and Parsing While
%   ======== === ======= ====

%------------------------------------------------------------------------------
% File scannew.pl
%---------  Parser  -----------------------------------------------------------

%
% Grammar
%
% a in AExp   arithmetic expressions
% b in BExp   boolean expressions
% S in Stmt   statements
%
% opa in OpA  arithmetic operators  (+,-,*,/)
% opb in OpB  boolean operators  (and, or)
% opr in OpR  relational operators (<, >, <=, >=, !=)
%
% x,y,..  in Var  variables
% n       in Num  numerals (integers)
% l       in Lab  labels
%

% a ::=  x | n | a1 opa a2
% b ::=  true | false | not b | b12 opb b2 | a1 opr a2
% S ::=  x := a | skip | { B  } | if b then S1 else S2 | if b then S1 | while b do S
% B ::=  S ; B | S
% B ::=  S ; B | S
% Program ::= B

% ----------------- prolog build parser 
program(AST) --> cmds(AST,1,_N).

cmds(Cmds,N1,N3) --> command(Cmd,N1,N2), {write(Cmd),nl}, restcmds(Cmd,Cmds,N2,N3).
  restcmds(Cmd,[Cmd|Cmds],N1,N2) --> [semicolon], cmds(Cmds,N1,N2).
  restcmds(Cmd,[Cmd],_N,_N)      --> [].

command(body(Cmds),N1,N2) --> [lbrace], cmds(Cmds,N1,N2),[rbrace].

command(while(bExpr(Test,N1),Body),N1,N3) -->
                       [while],boolexpr(Test), [do], {N2 is (N1+1)}, command(Body,N2,N3).

command(assign(V,E,N1),N1,N2) --> [ide(V)], [assign], expr(E), {N2 is (N1+1)}.

command(skip(N1),N1,N2) --> [skip], {N2 is N1+1}.

command(Cmd,N1,N4) --> [if],boolexpr(Test),[then], {N2 is (N1+1)} ,command(Then,N2,N3),restif(bExpr(Test,N1),Then,Cmd,N3,N4).
  restif(Test,Then,if(Test,Then,Else),N1,N2) --> [else],command(Else,N1,N2).
  restif(Test,Then,if(Test,Then),_N,_N) --> [].

expr(E) --> intexpr(E).
expr(E) --> boolexpr(E).

intexpr(E) --> term(T), restintexpr(T,E).
  restintexpr(T,E) --> weakop(Op), term(T1), restintexpr(exp(Op,T,T1),E).
  restintexpr(E,E) --> [].

term(T) --> element(P), restterm(P,T).
  restterm(P,T) --> strongop(Op), element(P1), restterm(exp(Op,P,P1),T).
  restterm(T,T) --> [].

element(num(N)) --> [num(N)].            element(ide(I)) --> [ide(I)].
element(E) --> [lparen], intexpr(E), [rparen].
element(minus(E)) --> [minus], element(E).

boolexpr(E) --> boolterm(T), restboolexpr(T,E).
  restboolexpr(T,E) --> [or], boolterm(T1), restboolexpr(exp(or,T,T1),E).
  restboolexpr(E,E) --> [].

boolterm(T) --> boolelement(P), restboolterm(P,T).
  restboolterm(P,T) --> [and], boolelement(P1), restboolterm(exp(and,P,P1),T).
  restboolterm(T,T) --> [].

boolelement(true) --> [true].             boolelement(false) --> [false].
boolelement(ide(I)) --> [ide(I)].
boolelement(E) --> comparison(E).
boolelement(E) --> [lparen], boolexpr(E), [rparen].
boolelement(not(E)) --> [not], [lparen], boolelement(E), [rparen].

comparison(exp(R,E1,E2)) --> intexpr(E1),rel(R),intexpr(E2).

rel(equal) --> [equal].  rel(neq) --> [neq].    rel(less) --> [less].
rel(grtr) --> [grtr].    rel(gteq) --> [gteq].  rel(lteq) --> [lteq].

weakop(plus) --> [plus].      weakop(minus) --> [minus].

strongop(times) --> [times].  strongop(divides) --> [divides].

orop(or) --> [or].  andop(and) --> [and].


%------------------------------------------------------------------------------
% Nielson Stuff
%
% UTILITY Predicates 
% ----------------------------------------------------------------------------

%----------------------------------------
% mymap (X,L1,L2)  is true when evey element 
%     E of L1 is paired as (X,E) in L2
%----------------------------------------
mymap(X,[H|T],[(X,H)|M]) :- 
   mymap(X,T,M).
mymap(X,[H],[(X,H)]).

%----------------------------------------
% mymapR (L1,X,L2)  is true when evey element 
%       E of L1 is paired as (E,X) in L2
%  Notice the pairing reversal compared to mymap
%----------------------------------------
mymapr([H|T],X,[(H,X)|M]) :- 
   mymapr(T,X,M).
mymapr([H],X,[(H,X)]).

%----------------------------------------
% append3 is true if L is the list append of L1, L2 and L3
%----------------------------------------
append3(L1,L2,L3,L) :- 
   append(L1,L2,LTmp), 
   append(LTmp,L3,L).

%----------------------------
% isLabel (N,S) is true if N is the label of S
%----------------------------
isLabel(N,assign(_x,_E,N)).   
isLabel(N,skip(N)).
isLabel(N,bExpr(_E,N)).

%----------------------------
% nthBlock(N,Blks,B) is true if B is a block in Blks
% with label N
%----------------------------
nthBlock(_N,[],[]).
nthBlock(N,[B|_Blks],B) :-
   isLabel(N,B).
nthBlock(N,[_B|Blks],BOut) :-
   nthBlock(N,Blks,BOut).



%----------------------------
% Complete these predicates
%----------------------------

   
%----------------------------
% final(Cmds,Ns) is true when list Ns contains all labels that are final labels in Cmds
%----------------------------
final(assign(_X,_E,N),[N]).
final(skip(N),[N]).
final(bExpr(_E,N),[N]).
final(while(_E,B),F) :- 
   final(B,F).
final(body(Cmds),F) :- 
   final(Cmds,F).
final([_C|Cmds],F) :- 
   final(Cmds,F).
final([C],F) :- 
   final(C,F).
final(if(_E,Then),F) :- 
   final(Then,F).
final(if(_E,Then,Else),F) :- 
   final(Then,F1),
   final(Else,F2),
   append(F1,F2,F).

%----------------------------
% init(Cmds,N)  is true when N is first label in Cmds
%----------------------------
init(assign(_X,_E,N),N).

   
   
%----------------------------   
% blocks(Cmds,Bs) is true when lsit Bs contains all blocks in Cmds   
%----------------------------
blocks(assign(X,E,N),[assign(X,E,N)]).



%----------------------------
% labels (S,L).  L is list of lables in S
%----------------------------
labels(assign(_X,_E,N),[N]).
 

%----------------------------
% flow (S,L).  L is list of pairs of flow (L1,L2)
%    where a statement C1 (with label L1) in S leads to 
%    another statement C2 (with label L2) in S
%----------------------------
flow([C|Cmds],F) :- 
   final(C,L1),
   init(Cmds,L2),
   mymapr(L1,L2,F1),
   flow(C,F2),
   flow(Cmds,F3),
   append3(F1,F2,F3,F).


%----------------------------
% flowR (L,LR).  LR is the reverse of L
% This predicate is complete
%----------------------------
flowR([(L1,L2)|T],[(L2,L1)|Tr]) :- flowR(T,Tr).
flowR([],[]).

%----------------------------
% freevarw(Exp,Vs) is true when list Vs contains the free variables of Exp
%----------------------------
freeVars(num(_N),[]).
freeVars(ide(V),[V]).

%----------------------------
% aExp(Exp,As) is true when list As contains all
% valid subexpressions in Exp
%----------------------------
aExp(exp(Op,T1,T2),As) :- 
   aExp(T1,A1),
   aExp(T2,A2),
   union3([exp(Op,T1,T2)],A1,A2,As).


%%%%%%%%%%%%%%%%
%  I have provided correct answers for AE, you need to provide
%  them for RD and LV and VB
%%%%%%%%%%%%%%%%   

%-----------------------------
% killAEVar(V,ExpsIn,ExpsOut) is true if ExpsOut
%  contains all expressions in ExpsIn that DO NOT
%  containe the free variable V
%-----------------------------
killAEVar(V,[Exp|Exps],ExpsOut) :- 
   freeVars(Exp,Vs),
   member(V,Vs),!,
   killAE(Exp,Exps,ExpsOut).
killAEVar(V,[Exp|Exps],[Exp|ExpsOut]) :- 
   % V is not in free variables of Exp due to previous clause, so we keep Exp
   killAEVar(V,Exps,ExpsOut).
killAEVar(_V,[],[]).

%----------------------------
% killAE(S,ExpsIn,ExpsOut) is true if ExpsOut contains
%    all Expressions of ExpsIn that are not killed by S1
%----------------------------
killAE(assign(V,_E,_N),AEIn,AEOut) :- 
   killAEVar(V,AEIn,AEOut).
killAE([Cmd|Cmds],AEIn,AEOut) :- 
   killAE(Cmd,AEIn,AETmp1),
   killAE(Cmds,AETmp1,AEOut).
killAE(_Cmd,_AEIn,[]).


%----------------------------
% genAE(S,Exps) is true if Exps is the list of subexpressions
%  generated by S
%---------------------------
genAE(assign(_X,E,_N),AEOut) :- 
   aExp(E,AEOut),!.
genAE(skip(_N),[]).
genAE(bExpr(E,_N),AEOut) :- 
   aExp(E,AEOut),!.
genAE([Cmd|Cmds],AEOut) :- 
   genAE(Cmd,A1),
   genAE(Cmds,As),
   union([A1],As,AEOut),!.   
genAE([],[]).


%----------------------
% Your turn for RD and LV and VB
%----------------------


%-- Scanner Utilities -----------------------------------------------------------------

	  go :- nl,write('>>> Interpreting: While programs <<<'), nl, nl,
      write('Enter name of source file: '), nl, getfilename(FileName), nl,
      see(FileName), scan(Tokens), seen, write('Scan successful'), nl, !,
      write(Tokens), nl, nl,!,
      program(Parse,Tokens,[eop]), write('Parse successful'), nl, !,
      write('Parse = '),write(Parse), nl, nl,	  
	  blocks(Parse,B),
	  write('Blocks = '),write(B),nl,nl,
	  labels(B,L),
	  write('Labels = '),write(L),nl,nl,
	  flow(Parse,F),
	  write('Flow = '),write(F),nl,nl,
	  flowR(F,Fr),
	  write('FlowR = '),write(Fr),nl,nl.
 

	  
%------------------------------------------------------------------------------

lower(C) :- 97=<C,C=<122.       % a-z
upper(C) :- 65=<C,C=<90.        % A-Z
digit(C)  :- 48=<C,C=<57.       % 0-9

space(32).     tabch(9).      period(46).      slash(47).     
endline(10).   endfile(26).   endfile(-1).     linefeed(13).

whitespace(C) :- space(C) ; tabch(C) ; endline(C) ; linefeed(C).

idchar(C) :- lower(C) ; digit(C).

filechar(C) :- lower(C) ; upper(C) ; digit(C) ; period(C) ; slash(C).

%------------------------------------------------------------------------------

getfilename(W) :- get0(C),restfilename(C,Cs),name(W,Cs).
   restfilename(C,[C|Cs]) :- filechar(C),get0(D),restfilename(D,Cs).
   restfilename(_C,[]).


%---------  Scanner  ----------------------------------------------------------

scan([T|Lt]) :- tab(4), getch(C), gettoken(C,T,D), restprog(T, D, Lt), !.

getch(C) :- get0(C), (endline(C),nl,tab(4) ; endfile(C),nl ; linefeed(C); put(C)).

restprog(eop, _C, []).        % end of file reached with previous character
restprog(_T,   C, [U|Lt]) :- gettoken(C, U, D), restprog(U, D, Lt).

  restid(C, [C|Lc], E) :- idchar(C), getch(D),restid(D,Lc,E).
  restid(C, [],     C).    % end identifier if C is not id character
  restnum(C, [C|Lc], E) :- digit(C), getch(D), restnum(D, Lc, E).
  restnum(C, [],     C).    % end number if C is not digit

gettoken(C, eop, 0) :- endfile(C).  
gettoken(C, T, D) :- single(C,T), getch(D).
gettoken(C, T, E) :- double(C,U),getch(D),(pair(C,D,T),getch(E) ; T=U,E=D).
gettoken(C, T, E) :- lower(C), getch(D), restid(D, Lc, E),   
                     name(I, [C|Lc]), (reswd(I),T=I ; T=ide(I)).
gettoken(C, num(N), E) :- digit(C), getch(D), restnum(D, Lc, E),  
                          name(N, [C|Lc]). 
gettoken(C, T, E) :- whitespace(C), getch(D), gettoken(D,T,E).
gettoken(C, _T, _E) :- write('Illegal character: '), put(C), nl, abort.

reswd(while).    reswd(do).       reswd(if).    
reswd(then).     reswd(else).     reswd(skip).     reswd(or).  
reswd(and).      reswd(true).     reswd(false).    reswd(not). 

single(40,lparen).   single(41,rparen).    single(42,times).
single(43,plus).     single(44,comma).     single(45,minus).
single(47,divides).  single(59,semicolon). single(61,equal).
single(123,lbrace).  single(125,rbrace).

double(58,colon).    double(60,less).      double(62,grtr).  
double(33,bang).

pair(58,61,assign).       % :=
pair(60,61,lteq).         % <=
pair(33,61,neq).          % !=
pair(62,61,gteq).         % >=

%-----------------------------------------------------------------------


