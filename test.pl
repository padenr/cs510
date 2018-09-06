answer('test1.txt',[ide(sum),assign,ide(sum),plus,ide(k),semicolon,ide(k),assign,ide(k),plus,num(1),semicolon,if,lparen,ide(k),less,num(0),rparen,then,ide(k),assign,num(0),minus,ide(k),else,skip,endif,while,lparen,ide(k),less,num(10),rparen,do,ide(sum),equal,ide(sum),plus,num(1),done,eop]).

answer('test2.txt',[ide(x),assign,num(124),semicolon,ide(x),assign,num(123),plus,num(456),semicolon,ide(x),assign,num(123),times,num(456),semicolon,ide(x),assign,num(456),minus,num(123),semicolon,ide(x),assign,num(123),divides,num(456),semicolon,ide(x),assign,num(123),plus,num(456),times,lparen,num(3),plus,num(6),rparen,semicolon,ide(x),assign,ide(y),semicolon,ide(x),assign,ide(y),plus,num(34),semicolon,ide(x),equal,ide(y),times,ide(z),semicolon,if,lparen,ide(x),less,num(123),rparen,then,ide(y),assign,num(0),else,ide(y),assign,num(1),endif,semicolon,if,lparen,ide(x),lteq,num(123),rparen,then,ide(y),assign,num(0),else,ide(y),assign,num(1),endif,semicolon,if,lparen,ide(x),grtr,num(123),rparen,then,ide(y),assign,num(0),else,ide(y),assign,num(1),endif,semicolon,if,lparen,ide(x),gteq,num(123),rparen,then,ide(y),assign,num(0),else,ide(y),assign,num(1),endif,semicolon,if,lparen,ide(x),equal,num(123),rparen,then,ide(y),assign,num(0),else,ide(y),assign,num(1),endif,semicolon,if,lparen,ide(x),neq,num(123),rparen,then,ide(y),assign,num(0),else,ide(y),assign,num(1),endif,semicolon,while,lparen,num(123),less,ide(z),rparen,do,ide(y),assign,ide(y),plus,num(1),done,while,lparen,num(123),lteq,ide(z),rparen,do,ide(y),assign,ide(y),plus,num(1),done,while,lparen,num(123),grtr,ide(z),rparen,do,ide(y),assign,ide(y),plus,num(1),done,while,lparen,num(123),gteq,ide(z),rparen,do,ide(y),assign,ide(y),plus,num(1),done,while,lparen,num(123),neq,ide(z),rparen,do,ide(y),assign,ide(y),plus,num(1),done,while,lparen,num(123),equal,ide(z),rparen,do,ide(y),assign,ide(y),plus,num(1),done,eop]).

tests :-
  forall(answer(F,L),
  (nl,write('==========================='),nl,
   see(F),scan(L1),seen,(L=L1,write(F),write(' :: Passes'),nl);
   (write(F),write(' :: Fails.  Your code returns :'),nl,write(L1),nl,
    write(' instead of :'),nl,write(L),nl))),
  write('==========================='),nl.
