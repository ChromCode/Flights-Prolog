% $Id: functions.pl,v 1.3 2016-11-08 15:04:13-08 - - $

not( X ) :- X, !, fail.
not( _ ).

mathfns( X, List ) :-
   S is sin( X ),
   C is cos( X ),
   Q is sqrt( X ),
   List = [S, C, Q].

constants( List ) :-
   Pi is pi,
   E is e,
   Epsilon is epsilon,
   List = [Pi, E, Epsilon].

sincos( X, Y ) :-
   Y is sin( X ) ** 2 + cos( X ) ** 2.

haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

timetoMins( Hours, Mins, Out) :-
   Hmin is Hours * 60,
   Out is Hmin + Mins.
   
addDist(F, T, Timemin, Out) :-
   airport(F, _, La1, Lo1),
   airport(T, _, La2, Lo2),
   toRads( La1, Ra1),
   toRads( Lo1, Ro1),
   toRads( La2, Ra2),
   toRads( Lo2, Ro2),
   haversine_radians( Ra1, Ro1, Ra2, Ro2, X),
   Speed is (X / 1.60934) / (500 / 60), 
   Out is round((Timemin + Speed)).


toRads(degmin(Deg, Min), Radian ) :-
   Rads is (Deg + ( Min / 60 )),
   Radian is Rads * (pi / 180).
   
printFlight(F, T, Hours, Mins) :-
   airport(F, N, _, _ ),
   write( 'depart '), write( F), write( ' '), write( N), write(' '), 
   (Hours < 10 -> write('0'), write( Hours) ; write( Hours) ), 
   (Mins < 10 -> write(':0'), write( Mins), nl ; write(':'), write(Mins), nl),
   airport(T, M, _, _ ),
   timetoMins( Hours, Mins, Tmr),
   addDist( F, T, Tmr, Ntmr),
   Nhour is truncate(Ntmr / 60),
   Nmin is mod(Ntmr, 60),
   write( 'arrive '), write( T), write( ' '), write( M), write(' '),
   (Nhour < 10 -> write('0'), write( Nhour) ; write(Nhour) ),
   (Nmin < 10 -> write( ':0'), write( Nmin), nl ; write( ':'), write( Nmin), nl).

isDay(Timemin) :-
   Timemin =< 1440.

   
isLater( H1, M1, H2, M2 ) :- 
   Hmin1 is H1 * 60,
   Hmin2 is H2 * 60,
   T1 is Hmin1 + M1,
   T2 is Hmin2 + M2 + 30,
   T1 > T2.


displayList([]) :-
   nl.
   
displayList([Hd|Tl]) :-
   display(Hd), nl,
   displayList(Tl).

displayFlist([], _) :-
   nl.
   
displayFlist(_, []) :-
   nl.
   
displayFlist([F, T|Tl], [Tmin|Nxt]) :-
   Hours is truncate(Tmin / 60),
   Mins is mod(Tmin, 60),
   printFlight(F, T, Hours, Mins),
   displayFlist([T|Tl], Nxt).

fly( L, M ) :- 
   flying( L, M, [L], 0, 0, Outlist, Timelist ),
   !,
   displayFlist(Outlist, Timelist).

flying( L, L, _, _, _, [L], _ ).   
flying( L, M, Path, H2, M2, [L|List], [Tmin|Tlist] ) :-
   flight( L, X, time( Hours, Mins) ),
   not( member( X, Path)),
   isLater( Hours, Mins, H2, M2 ),
   timetoMins( Hours, Mins, Tmin),
   addDist(L, X, Tmin, Ntime),
   isDay(Tmin),
   Nhours is truncate(Ntime/60),
   Nmins is mod(Ntime, 60),
   flying( X, M, [L|Path], Nhours, Nmins, List, Tlist ).
   


