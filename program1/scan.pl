
%   Scanning Wren
%   ======== ====

% File: scan

%------------------------------------------------------------------------------

go :- nl,write('>>> Scanning Wren <<<'), nl, nl,
      write('Enter name of source file:  '), nl, getfilename(File), nl,
      see(File), scan(Tokens), seen, write('Scan successful'), nl, nl,
      write(Tokens), nl.

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
  restfilename(C,[]).

%------------------------------------------------------------------------------
%---------  Scanner  ----------------------------------------------------------

scan([T|Lt]) :- tab(4), getch(C), gettoken(C,T,D), restprog(T, D, Lt), !.

getch(C) :- get0(C), (endline(C),nl,tab(4) ; endfile(C),nl ; linefeed(C); put(C)).

restprog(eop, C, []).        % end of file reached with previous character
restprog(T,   C, [U|Lt]) :- gettoken(C, U, D), restprog(U, D, Lt).

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
gettoken(C, T, E) :- write('Illegal character: '), put(C), nl, abort.

reswd(program).  reswd(is).       reswd(begin).    reswd(end).
reswd(var).      reswd(integer).  reswd(boolean).  reswd(read).
reswd(write).    reswd(while).    reswd(do).       reswd(if).
reswd(then).     reswd(else).     reswd(skip).     reswd(or).
reswd(and).      reswd(true).     reswd(false).    reswd(not).
reswd(xor).      reswd(endif).    reswd(done).

single(40,lparen).   single(41,rparen).    single(42,times).
single(43,plus).     single(44,comma).     single(45,minus).
single(47,divides).  single(59,semicolon). single(61,equal).

double(58,colon).    double(60,less).      double(62,grtr).
double(33,exclamation).

pair(58,61,assign).       % :=
pair(60,61,lteq).         % <=
pair(33,61,neq).          % !=
pair(62,61,gteq).         % >=

%-----------------------------------------------------------------------
