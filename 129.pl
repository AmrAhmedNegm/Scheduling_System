ta_slot_assignment([],[],_).
ta_slot_assignment([ta(X,Y)|T],[ta(X,Y1)|T],X):- Y1 is Y -1.
ta_slot_assignment([ta(Y,Z)|T],[ta(Y,Z)|T1],X):- Y\=X,ta_slot_assignment(T,T1,X).
	
	
slot_assignment(0,X,X,[]).
slot_assignment(N,X,Y,Assignment):-permutation(X,Acc),length(Assignment,N),helperS(N,X,Y,Acc,Assignment).

helperS(0,_,Acc,Acc,[]).
helperS(N,X,Y,Acc,[TA|T]):-select(ta(TA,L),X,Rem),L>0,ta_slot_assignment(Acc,NAcc,TA),helperS(N1,Rem,Y,NAcc,T),N is N1+1.

countE(_,[],0).
countE(X,[X],1).
countE(X,[H|T],C):-  is_list(H),member(X,H),countE(X,T,C1),C is C1 +1.
countE(X,[H|T],C):-  is_list(H),\+member(X,H),countE(X,T,C).


max_slots_per_day(DaySched, Max) :-
    flatten(DaySched, TAs),
    max_slots_per_day_helper(TAs, DaySched, Max).

max_slots_per_day_helper([], _, _).
max_slots_per_day_helper([TA|TAs], DaySched, Max) :-
    countE(TA,DaySched,Count),
    Count =< Max,
    max_slots_per_day_helper(TAs,DaySched,Max).
	
day_schedule([],_,_,[]).
day_schedule(Day,TAs,RemTA,Assignment):- helperDay(Day,TAs,RemTA,TAs,Assignment).

helperDay([],_,Acc,Acc,[]).
helperDay([H|T],X,RemTA,Acc,[H2|T2]):-slot_assignment(H,Acc,NAcc,H2),same_order(X,NAcc),helperDay(T,X,RemTA,NAcc,T2).

same_order([],[]).
same_order([ta(X,_)|T],[ta(X,_)|T2]):- same_order(T,T2).
	
week_schedule([],_,_,[]).
week_schedule([H|T],TAs,DayMax,[H2|T2]):- 
  day_schedule(H,TAs,NTAs,H2),
  max_slots_per_day(H2,DayMax),
  week_schedule(T,NTAs,DayMax,T2).

