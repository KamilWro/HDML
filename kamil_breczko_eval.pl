:- module(kamil_breczko_eval, [run/5]).

:- op(200, fx, ~).
:- op(500, xfy, v).

run(Program, FName, Arg, Value, Clauses) :-
	choose_def(Program, FName, def(FName,P,E)),
	environment(P,Arg,[],Environment),
	expression(E,Environment,Program, Value, Clauses).
	  
%===================================================================================================
%                                            srodowisko
%===================================================================================================
environment(pair(_,P1,P2),(H,T),Acc,Env):-
	environment(P1,H,Acc,Acc1),
	environment(P2,T,Acc1,Env),!.
	
environment(var(_,X),H,Acc,[(X-H)|RestAcc]):-
	( memberchk(X-_,Acc) ->
		selectchk(X-_,Acc,RestAcc)
	; RestAcc=Acc
	).	
environment(wildcard(_),_,Acc,Acc).

environment(pair(PosThrow,_,_),_,_,_):-
	throw(runtime_error("Mismatched pattern", PosThrow)).



%===================================================================================================
%                                            Wyrazenia				
%===================================================================================================
% wyrazenie(wyrazenie,srodowisko,lista_definicji, wartosc, efekt uboczny)


%---------------------------------------------------------------------------------------------------
%                                        Wyrazenia atomowe
%---------------------------------------------------------------------------------------------------
expression(empty(_),_,_,[],[]).

expression(num(_,Num),_,_,Num,[]).

expression(var(_,Var),Environment,_,Values,[]):-
	memberchk(Var-Values,Environment),!.
expression(var(PosThrow,_),_,_,_,_):-
	throw(runtime_error("Variable was not declared in this scope", PosThrow)).
	
expression(bit(_,E),Environment,Program,Values,Clauses):-
	expression(E,Environment,Program,Num,C),	
	gensym(wire,L),
	Values=[L],
	(Num=1 ->
		C0=[L]
	;Num=0 ->
		C0=[~L]	
	),!,
	append(C,C0,Clauses).
expression(bit(PosThrow,_),_,_,_,_):-
	throw(runtime_error("segmentation fault", PosThrow)).	
	
%---------------------------------------------------------------------------------------------------
%                                            Wybor bitow
%---------------------------------------------------------------------------------------------------
% Wazne
% Specyfikacja wyboru bitow w tresci zadania nie rozstrzyga przypadku, jesli przy wyborze bitow Num2 jest 
% wieksze od Num1 (lub Num2 jest mniejszy od 0 czy tez Num1 jest wiekszy od rozmiaru wektora). 
% Zakladam ze interpreter powinien sie zachowac podobnie jak odwolanie sie do indeksu
% spoza tablicy czy listy, czyli jesli wystapi odwolania do wektora spoza zakresu program powinien zawodzic.
% W przypadku gdy Num2 jest wieksze od Num1, program zwroci pusta liste (zachowujac powyzsza regule)
	
expression(bitsel(_,E1,E2),Environment,Program,Values,Clauses):-
	expression(E1,Environment,Program,Signals,C1),
	expression(E2,Environment,Program,Num,C2),
	is_list(Signals),
	number(Num),
	Pos is Num +1,
	nth1(Pos, Signals, Signal),!,
	Values=[Signal],
	append(C1,C2,Clauses).
expression(bitsel(PosThrow,_,_),_,_,_,_):-
	throw(runtime_error("segmentation fault", PosThrow)).	

expression(bitsel(_,E1,E2,E3),Environment,Program,Values,Clauses):-
	expression(E1,Environment,Program,Signals,C1),
	expression(E2,Environment,Program,Num1,C2),
	expression(E3,Environment,Program,Num2,C3),
	is_list(Signals),
	number(Num1),
	number(Num2),
	select(Signals,Num2,Num1,Values),!,
	append(C1,C2,C),
	append(C,C3,Clauses).
expression(bitsel(PosThrow,_,_,_),_,_,_,_):-
	throw(runtime_error("segmentation fault", PosThrow)).

%---------------------------------------------------------------------------------------------------
%                                            Para
%---------------------------------------------------------------------------------------------------
expression(pair(_,E1,E2),Environment,Program,Values,Clauses):-
	expression(E1,Environment,Program,V1,C1),
	expression(E2,Environment,Program,V2,C2),!,
	append(C1,C2,Clauses),
	Values=(V1,V2).
expression(pair(PosThrow,_,_,_),_,_,_,_):-
	throw(runtime_error("incorrect expression", PosThrow)).
	
%---------------------------------------------------------------------------------------------------
%                                    Operacje na sygnalach	
%---------------------------------------------------------------------------------------------------

expression(op(_,Op,E1,E2),Environment,Program,Values,Clauses):-
	memberchk(Op,['&','|','^']),
	expression(E1,Environment,Program,Signals1,C1),
	expression(E2,Environment,Program,Signals2,C2),
	is_list(Signals1),
	is_list(Signals2),
	append(C1,C2,CN),
	concate(Signals1,Signals2,Op,Signals),
	phrase(clauses(C,Values),Signals),!,
	append(CN,C,Clauses).

expression(op(_,'~',E),Environment,Program,Values,Clauses):-
	expression(E,Environment,Program,Signals,C1),
	is_list(Signals),
	phrase(clauses(C2,Values),Signals),!,
	append(C1,C2,Clauses).

expression(op(_,'@',E1,E2),Environment,Program,Values,Clauses):-
	expression(E1,Environment,Program,Signals1,C1),
	expression(E2,Environment,Program,Signals2,C2),
	is_list(Signals1),
	is_list(Signals2),!,	
	append(C1,C2,Clauses),
	append(Signals2,Signals1,Values).

expression(op(_,'#',E),Environment,Program,Values,Clauses):-
	expression(E,Environment,Program,Signals,Clauses),
	is_list(Signals),!,
	length(Signals,Values).
		
%---------------------------------------------------------------------------------------------------
%                                      Operacje na liczbach	
%---------------------------------------------------------------------------------------------------
expression(op(_,'-',E),Environment,Program,Values,Clauses):-
	expression(E,Environment,Program,Num,Clauses),
	number(Num),!,
	Values is - Num.


expression(op(_,Op,E1,E2),Environment,Program,Values,Clauses):-
	memberchk(Op,['+','-','/','*','%']),
	expression(E1,Environment,Program,Num1,C1),
	expression(E2,Environment,Program,Num2,C2),
	number(Num1),
	number(Num2),
	append(C1,C2,Clauses),
	(
	  Op='+',!, Values is Num1 + Num2
	; Op='-',!, Values is Num1 - Num2
	; Op='/', Num2 =\= 0, !, Values is Num1 // Num2
	; Op='*',!, Values is Num1 * Num2
	; Op='%', Num2 =\= 0, !, Values is Num1 mod Num2
	).

expression(op(_,Op,E1,E2),Environment,Program,Values,Clauses):-
	memberchk(Op,['<','>','<=','>=','=','<>']),
	expression(E1,Environment,Program,Num1,C1),
	expression(E2,Environment,Program,Num2,C2),
	number(Num1),
	number(Num2),	
	append(C1,C2,Clauses),
	(
	  Op='<', !, 
	  		( Num1 < Num2 ->
	  			Values=1
	  		; Values=0
	  		)
	; Op='>', !,
		  	( Num1 > Num2 ->
	  			Values=1
	  		; Values=0
	  		)
	; Op='<=',!,
		  	( Num1 =< Num2 ->
	  			Values=1
	  		; Values=0
	  		)
	; Op='>=',!, 
		  	( Num1 >= Num2 ->
	  			Values=1
	  		; Values=0
	  		)
	; Op='=', !,
	  		( Num1 =:= Num2 ->
	  			Values=1
	  		; Values=0
	  		)
	; Op='<>',
	  		( Num1 =\= Num2 ->
	  			Values=1
	  		; Values=0
	  		)
	).
	
expression(op(PosThrow,Op,_,_),_,_,_,_):-
	memberchk(Op,['<','>','<=','>=','=','<>','+','-','/','*','%']),
	throw(runtime_error("expected number", PosThrow)).
expression(op(PosThrow,Op,_,_),_,_,_,_):-
	memberchk(Op,['&','|','^','@']),
	throw(runtime_error("expected signals", PosThrow)).
expression(op(PosThrow,'-',_),_,_,_,_):-
	throw(runtime_error("expected number", PosThrow)).
expression(op(PosThrow,Op,_),_,_,_,_):-
	memberchk(Op,['~','#']),
	throw(runtime_error("expected signals", PosThrow)).
%---------------------------------------------------------------------------------------------------
%                                          Wyrazenie if
%---------------------------------------------------------------------------------------------------
expression(if(_,E1,E2,E3),Environment,Program,Values,Clauses):-
	expression(E1,Environment,Program,Num,C1),
	number(Num),
	(Num=0 ->
		expression(E3,Environment,Program,Values,C2)
	; expression(E2,Environment,Program,Values,C2)
	),!,
	append(C1,C2,Clauses).
expression(if(PosThrow,_,_,_),_,_,_,_):-
	throw(runtime_error("invalid if expression", PosThrow)).
	
%---------------------------------------------------------------------------------------------------
%                                          Wyrazenie let
%---------------------------------------------------------------------------------------------------

expression(let(_,P,E1,E2),Environment,Program,Values,Clauses):-
	expression(E1,Environment,Program,V1,C1),
	environment(P,V1,Environment,NewEnvironment),
	expression(E2,NewEnvironment,Program,Values,C2),!,
	append(C1,C2,Clauses).
expression(let(PosThrow,_,_,_),_,_,_,_):-
	throw(runtime_error("invalid let expression", PosThrow)).

%---------------------------------------------------------------------------------------------------
%                                     Wyrazenie <wywolanie funkcji>
%---------------------------------------------------------------------------------------------------
expression(call(_, Name, E),Environment,Program,Values,Clauses):-
	expression(E,Environment,Program,V,C1),
	choose_def(Program, Name, def(Name,Pdef,Edef)),
	environment(Pdef,V,Environment,NewEnvironment),
	expression(Edef,NewEnvironment,Program,Values,C2),!,
	append(C1,C2,Clauses).
expression(call(PosThrow,_,_),_,_,_,_):-
	throw(runtime_error("invalid function call", PosThrow)).
	

%===================================================================================================
%                                        Produkcja klauzul			
%===================================================================================================
clauses(Values, Clauses) -->
	[s(Op,B,C)],
	{ 
	  B= ~X,C= ~Y,
	( Op='&', !,
		gensym(wire,A), V1= A v X v Y, V2= ~A v B, V3= ~A v C,
		Values=[V1,V2,V3|RestV],
		Clauses=[A|RestC]
	; Op='|',!,
		gensym(wire,A), V1= ~A v B v C, V2= A v X, V3= A v Y,
		Values=[V1,V2,V3|RestV],
		Clauses=[A|RestC]	
	; Op='^',
		gensym(wire,A), V1= A v X v C,V2= A v B v Y, V3= ~A v B v C,V4= ~A v X v Y,
		Values=[V1,V2,V3,V4|RestV],
		Clauses=[A|RestC]
	)			
	},clauses(RestV,RestC).
	
clauses(Values, Clauses) -->
	[s(Op,B,C)],
	{
	  \+ B= ~_, C= ~Y,
	( Op='&', !,
		gensym(wire,A), V1= A v ~B v Y, V2= ~A v B, V3= ~A v C,
		Values=[V1,V2,V3|RestV],
		Clauses=[A|RestC]
	; Op='|',!,
		gensym(wire,A), V1= ~A v B v C, V2= A v ~B, V3= A v Y,
		Values=[V1,V2,V3|RestV],
		Clauses=[A|RestC]	
	; Op='^',
		gensym(wire,A), V1= A v ~B v C,V2= A v B v Y, V3= ~A v B v C,V4= ~A v ~B v Y,
		Values=[V1,V2,V3,V4|RestV],
		Clauses=[A|RestC]
	)			
	},clauses(RestV,RestC).	

clauses(Values, Clauses) -->
	[s(Op,B,C)],
	{
	  B= ~X, \+ C= ~_,
	( Op='&', !,
		gensym(wire,A), V1= A v X v ~C, V2= ~A v B, V3= ~A v C,
		Values=[V1,V2,V3|RestV],
		Clauses=[A|RestC]
	; Op='|',!,
		gensym(wire,A), V1= ~A v B v C, V2= A v X, V3= A v ~C,
		Values=[V1,V2,V3|RestV],
		Clauses=[A|RestC]	
	; Op='^',
		gensym(wire,A), V1= A v X v C,V2= A v B v ~C, V3= ~A v B v C,V4= ~A v X v ~C,
		Values=[V1,V2,V3,V4|RestV],
		Clauses=[A|RestC]
	)			
	},clauses(RestV,RestC).	

clauses(Values, Clauses) -->
	[s(Op,B,C)],
	{
	  \+ B= ~_, \+ C= ~_,
	( Op='&', !,
		gensym(wire,A), V1= A v ~B v ~C, V2= ~A v B, V3= ~A v C,
		Values=[V1,V2,V3|RestV],
		Clauses=[A|RestC]
	; Op='|',!,
		gensym(wire,A), V1= ~A v B v C, V2= A v ~B, V3= A v ~C,
		Values=[V1,V2,V3|RestV],
		Clauses=[A|RestC]	
	; Op='^',
		gensym(wire,A), V1= A v ~B v C, V2= A v B v ~C, V3= ~A v B v C,V4= ~A v ~B v ~C,
		Values=[V1,V2,V3,V4|RestV],
		Clauses=[A|RestC]
	)			
	},clauses(RestV,RestC).	
	
	
clauses(Values,Clauses) -->
	[B],
	{	\+ B=s(_,_,_),
		B= ~X,
		gensym(wire,A), V1= ~A v X, V2= A v B,
		Values=[V1,V2|RestV],
		Clauses=[A|RestC]			
	},clauses(RestV,RestC).	
clauses(Values,Clauses) -->
	[B],
	{	\+ B=s(_,_,_),
		\+ B= ~_,
		gensym(wire,A), V1= ~A v ~B, V2= A v B,
		Values=[V1,V2|RestV],
		Clauses=[A|RestC]			
	},clauses(RestV,RestC).
clauses([],[]) -->
	[].
	
%===================================================================================================
%                                        Pomocnicze				
%===================================================================================================

%---------------------------------------------------------------------------------------------------
%                        Wybieranie listy zaczynajacej sie od FROM do TO
%---------------------------------------------------------------------------------------------------

select(List,FROM,TO,Res):-
	length(List,Length),
	Length>FROM, 	
	next(List,0,FROM,NewList),
	take(NewList,FROM,TO,Res).
	
take([H|T],Count,TO,[H|Res]):-
	Count<TO,!,
	NewCount is Count + 1,
	take(T,NewCount, TO, Res).
take([H|_],TO,TO,[H]):-!. 
take(_,Count,TO,[]):-
	Count>TO.	

next([_|T],Count, FROM, NewList):-
	Count<FROM,!,
	NewCount is Count + 1,
	next(T,NewCount, FROM, NewList).
next(NewList,FROM, FROM, NewList).

%---------------------------------------------------------------------------------------------------
%                            laczy dwie listy z podanym operatorem
%---------------------------------------------------------------------------------------------------
concate([],[],_,[]):- !.
concate([],[_],_,[]):- !.
concate([_],[],_,[]):- !.
concate([H1|T1],[H2|T2],Op,[s(Op,H1,H2)|List]):-
	concate(T1,T2,Op,List).
	
%---------------------------------------------------------------------------------------------------
%                                    Wybor definicji
%---------------------------------------------------------------------------------------------------	
choose_def([def(Name,P,E)|_],Name, def(Name,P,E)):-!.
choose_def([_|T],Name, Def):-
	choose_def(T,Name,Def).
	
choose_def([def(_,pair(file(File,_,_,_,_),_,_),_)|_],_,_):-
	throw(runtime_error("function not found", file(File,1,1,0))).
choose_def([def(_,var(file(File,_,_,_,_),_),_)|_],_,_):-
	throw(runtime_error("function not found", file(File,1,1,0))).
choose_def([def(_,wildcard(file(File,_,_,_,_)),_)|_],_,_):-
	throw(runtime_error("function not found", file(File,1,1,0))).


