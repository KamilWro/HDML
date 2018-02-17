:- module(kamil_breczko_solver, [solve/2]).

% definiujemy operatory ~/1 oraz v/2
:- op(200, fx, ~).
:- op(500, xfy, v).

% Główny predykat rozwiązujący zadanie.
solve(Clauses, Solution) :-
	simple(Clauses,[],ComplexClauses,[],Valuation),
	complex(ComplexClauses,[],RestClauses, Valuation, [], GeneralValuation),
	setValue(RestClauses,Valuation,NewValuation),	
	merge(GeneralValuation,NewValuation,Solution).

%--------------------------------------------------------------------------------------------------
%                                          Predykat simple/5
%--------------------------------------------------------------------------------------------------
% Wstępne przesortowanie klauzul. Nadanie wartościowań zmiennym tym które występują jako pojedynczy 
% literał w klauzuli.

simple([],AccComplex,AccComplex,AccVal,AccVal).
simple([[]|_],_,_,_,_):-!,fail.
simple([HClouse|TClauses],AccComplex,ComplexClauses,AccVal,Valuation):-
	\+ check(HClouse),
	\+ checkLiteral(HClouse,AccVal,_),!,
	addLiteralTrue(HClouse,AccVal,Acc),
	simple(TClauses,AccComplex,ComplexClauses,Acc,Valuation).
simple([HClause|TClauses],AccComplex,ComplexClauses,AccVal,Valuation):-
	\+ check(HClause),
	checkLiteral(HClause,AccVal,t),!,
	simple(TClauses,AccComplex,ComplexClauses,AccVal,Valuation).		
simple([HClause|TClauses],AccComplex,ComplexClauses,AccVal,Valuation):-
	check(HClause),
	simple(TClauses,[HClause|AccComplex],ComplexClauses,AccVal,Valuation).	


%--------------------------------------------------------------------------------------------------
%                                       Predykat complex/6
%--------------------------------------------------------------------------------------------------
% Sprawdzanie spełnialności klauzul złożonych, wyjmowane są te klauzule z listy które są spełnialne 
% zawsze a literały w niej zapamiętane.

complex([],AccList,AccList,_,AccGeneral,AccGeneral).
complex([HClause|TClauses],AccList,ListClauses,Valuation,AccGeneral,GeneralValuation):-
	stripClouses([HClause],[],Clause,[],Variables,Valuation,IsTrue,IsNeutral),
	( 
	  ( \+ var(IsTrue) ; excluded_middle(Clause) ) -> 
	    (sort(Variables,SortVariables),
	     maplist(addGeneral,SortVariables,GeneralVariables),
	     merge(GeneralVariables,AccGeneral,General),
	     complex(TClauses,AccList,ListClauses,Valuation,General,GeneralValuation)
	    )
	; \+ var(IsNeutral) ->
	    (sort(Clause,SortClause),
	     complex(TClauses,[SortClause|AccList],ListClauses,Valuation,AccGeneral,GeneralValuation)
	    )
	).

%--------------------------------------------------------------------------------------------------
%                                   Predykat stripClouses/8
%--------------------------------------------------------------------------------------------------
% Rozbiera klauzule na literały, sprawdzając czy klauzula złożona jest spełniona/sprzeczna. 

stripClouses([],AccRest,AccRest,AccVar,AccVar,_,_,_):-!.
stripClouses([HClause],AccRest,[HClause|AccRest],AccVar,AccVar,Valuation,IsTrue,IsNeutral):-
	\+ check(HClause),
	isBool(HClause,Valuation,IsTrue,_,IsNeutral),!.
stripClouses([HClause],AccRest,[HClause|AccRest],AccVar,[X|AccVar],_,_,x):-
	\+ check(HClause),!,
	choose(HClause,X).
stripClouses([HClause v TClause],AccRest,RestClause,AccVar,Variables,Valuation,IsTrue,IsNeutral):-
	isBool(HClause,Valuation,IsTrue,_,IsNeutral),!,
	stripClouses([TClause],[HClause|AccRest],RestClause,AccVar,Variables,Valuation,IsTrue,IsNeutral).
stripClouses([HClause v TClause],AccRest,RestClause,AccVar,Variables,Valuation,IsTrue,x):-
	choose(HClause,X),
	stripClouses([TClause],[HClause|AccRest],RestClause,[X|AccVar],Variables,Valuation,IsTrue,x).
			

%--------------------------------------------------------------------------------------------------
%                                       Predykat setValue/3
%--------------------------------------------------------------------------------------------------
% Nadanie wartości zmiennym w klauzuli

setValue([],Valuation,Valuation):-!.
setValue(RestClauses,Valuation,FinalValuation):-
	setValue(RestClauses,Valuation,NewValuation,[],GeneralValuation),
	merge(GeneralValuation,NewValuation,FinalValuation).	

%--------------------------------------------------------------------------------------------------
setValue([],Valuation,Valuation,GeneralValuation,GeneralValuation).
setValue([[]|TRest],Valuation,NewValuation,AccGeneral,GeneralValuation):-!,
	setValue(TRest,Valuation,NewValuation,AccGeneral,GeneralValuation).

setValue([[HLiteral|TLiteral]|TRest],Valuation,NewValuation,AccGeneral,GeneralValuation):-
	TLiteral=[_|_],
	\+ checkLiteral(HLiteral,Valuation,_),
	addLiteralFalse(HLiteral,Valuation,Acc),
	checkClauses([TLiteral|TRest],TRest2,Acc,[],General),
	merge(General,AccGeneral,AccGeneral2),
	setValue(TRest2,Acc,NewValuation,AccGeneral2,GeneralValuation).

setValue([[HLiteral|TLiteral]|TRest],Valuation,NewValuation,AccGeneral,GeneralValuation):-
	simple([HLiteral],[],_,Valuation,Acc),
	checkClauses([[HLiteral|TLiteral]|TRest],TRest2,Acc,[],General),!,
	merge(General,AccGeneral,AccGeneral2),
	setValue(TRest2,Acc,NewValuation,AccGeneral2,GeneralValuation).

setValue([[HLiteral|TLiteral]|TRest],Valuation,NewValuation,AccGeneral,GeneralValuation):-
	TLiteral=[_|_],	
	checkLiteral(HLiteral,Valuation,_),!,
	setValue([TLiteral|TRest],Valuation,NewValuation,AccGeneral,GeneralValuation).


%--------------------------------------------------------------------------------------------------
%                                      Predykat checkClauses/5
%--------------------------------------------------------------------------------------------------
% Pomocniczy predykat do setValue/5, usuwa klazule spełnione zachowując ich zmienne.

checkClauses([],[],_,AccGeneral,AccGeneral).
checkClauses([HClause|TClauses],RestClauses,Valuation,AccGeneral,GeneralValuation):-
	select(HClause,Valuation,Variables,IsTrue,IsNeutral),
	( \+ var(IsTrue) ->
	    (sort(Variables,SortVariables),	
	     maplist(addGeneral,SortVariables,GeneralVariables),
	     merge(GeneralVariables,AccGeneral,General),
	     checkClauses(TClauses,RestClauses,Valuation,General,GeneralValuation)
	    )
	; \+ var(IsNeutral) ->
	    (RestClauses=[HClause|Clauses],
	     checkClauses(TClauses,Clauses,Valuation,AccGeneral,GeneralValuation)
	    )
	).

%--------------------------------------------------------------------------------------------------
%                                           Predykat select/5
%--------------------------------------------------------------------------------------------------
% Sprawdza czy klauzula jest spełniona/sprzeczna

select([],_,[],_,_):-!.
select([HLiteral|TLiteral],Valuation,Variables,IsTrue,IsNeutral):-
	isBool(HLiteral,Valuation,IsTrue,_,_),!,	
	select(TLiteral,Valuation,Variables,IsTrue,IsNeutral).
select([HLiteral|TLiteral],Valuation,Variables,IsTrue,x):-	
	choose(HLiteral,X),	
	Variables=[X|Acc],
	select(TLiteral,Valuation,Acc,IsTrue,x).

%--------------------------------------------------------------------------------------------------
%                                       Predykat excluded_middle/1
%--------------------------------------------------------------------------------------------------
% Sprawdza czy klauzula podlega prawu wyłączonego środka

excluded_middle([HLiteral|TLiteral]):-
	excluded_middle(HLiteral,TLiteral),!.
excluded_middle([_|TLiteral]):-
	excluded_middle(TLiteral).	

excluded_middle(H,[~H|_]):-!.
excluded_middle(~H,[H|_]):-!.
excluded_middle(H,[_|T]):-
	excluded_middle(H,T).

%--------------------------------------------------------------------------------------------------
%                                       Obsługa listy wartościowań
%--------------------------------------------------------------------------------------------------

addTrue(Clause, (Clause, t)).
addFalse(Clause, (Clause, f)).
addGeneral(Clause, (Clause, x)).

% Dodaje do listy wartościowań zmienną o wartościowaniu _.
addLiteralGeneral(Literal,Valuation,NewValuation):-
	choose(Literal,X),
	addGeneral(X,General),
	addLiteral(General,Valuation,NewValuation).
addLiteralTrue(~Literal,Valuation,NewValuation):-
	!,addFalse(Literal,X),
	addLiteral(X,Valuation,NewValuation).
addLiteralTrue(Literal,Valuation,NewValuation):-	
	addTrue(Literal,X),
	addLiteral(X,Valuation,NewValuation).
addLiteralFalse(~Literal,Valuation,NewValuation):-
	!,addTrue(Literal,X),
	addLiteral(X,Valuation,NewValuation).
addLiteralFalse(Literal,Valuation,NewValuation):-	
	addFalse(Literal,X),
	addLiteral(X,Valuation,NewValuation).

%
addLiteral((H1,V),[],[(H1,V)]):-!.
addLiteral((H1,V1),[(H2,V2)|T],[(H1,V1),(H2,V2)|T]):-
	H1@<H2,!.
addLiteral((H1,V1),[(H2,V2)|T],[(H2,V2)|Valuation]):-
	H1@>H2,
	addLiteral((H1,V1),T,Valuation).	

% Sprawdza czy literał znajduje się w liście wartościowań.
checkLiteral(~H,Valuation,Bool):-
	!,find(H,Valuation,X),
	( X=f ->
	   Bool=t
	; X=t ->
	   Bool=f
	; Bool=x
	).
checkLiteral(H,Valuation,Bool):-
	\+ H= ~_,
	find(H,Valuation,Bool).

%
find(H,[(H,V)|_],V):-!.
find(H1,[(H2,_)|T],V):-
	H1 @> H2,
	find(H1,T,V).

% Sprawdza wartościowanie literału
isBool(H,Valuation,IsTrue,IsFalse,IsGeneral):-
	checkLiteral(H,Valuation,V),
	( V=t ->
	   IsTrue=t
	; V=f ->
	   IsFalse=f
	; V=x ->
	   IsGeneral=x
	).

%--------------------------------------------------------------------------------------------------
%                                       Pomocnicze predykaty
%--------------------------------------------------------------------------------------------------

% Scalanie dwóch list, przy czym kiedy elementy w pierwszej liście powtarzają się w drugiej liście, 
% brane jest pod uwage tylko te elementy z drugiej listy.
merge([], B, B):-!.
merge(A, [], A):-!.
merge([(X1,V1)|A], [(X2,V2)|B], [(X1,V1)|Rs]) :-
	X1@<X2,!,
        merge(A,[(X2,V2)|B], Rs).
merge([(X1,V1)|A], [(X2,V2)|B], [(X2,V2)|Rs]) :-
	X2@<X1,!,
        merge([(X1,V1)|A],B, Rs).
merge([(X,_)|A], [(X,V)|B], [(X,V)|Rs]) :-
        merge(A,B, Rs).

%Sprawdza czy klauzula jest złożona
check(_ v _).

%Wybiera zmienną z literału
choose(~X,X):-!.
choose(X,X):- 
	\+ X= ~_.


