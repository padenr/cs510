intersection([], _, []).
intersection([Head|L1tail], L2, L3) :-
   memberchk(Head, L2),
   !,
   L3 = [Head|L3tail],
   intersection(L1tail, L2, L3tail).
intersection([_|L1tail], L2, L3) :-
   intersection(L1tail, L2, L3).

intersectionN([],[]).
   
intersectionN([Set|Sets],Ins) :-
   intersectionN(Sets,Ins1),	
   intersection(Set,Ins1,Ins).
   
	


union([], L, L).
union([Head|L1tail], L2, L3) :-
   memberchk(Head, L2),
   !,
   union(L1tail, L2, L3).
union([Head|L1tail], L2, [Head|L3tail]) :-
   union(L1tail, L2, L3tail).
		

union3(L1,L2,L3,L) :- 
   union(L1,L2,LTmp), 
   union(LTmp,L3,L).