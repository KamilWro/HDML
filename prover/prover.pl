:- module(prover, [resolve/4, prove/2]).

% definiujemy operatory ~/1 oraz v/2
:- op(200, fx, ~).
:- op(500, xfy, v).

% -------------------------------------------------------------------------------------------------
%                                        Szukanie rezolwenty
% -------------------------------------------------------------------------------------------------

resolve(Var, Clause1, Clause2 ,Resolvent):-
	clause_to_list(Clause1,[],ListClause1),
	clause_to_list(Clause2,[],ListClause2),
	sort(ListClause1,SortClause1),
	sort(ListClause2,SortClause2),
	delete(SortClause1,Var,Del1),
	delete(SortClause2,~Var,Del2),
	merge_set(Del1, Del2, ResolventList),
	list_to_clause(ResolventList,Resolvent).

clause_to_list([],_,[]):-!.
clause_to_list(Literal,AccList,List):-
	\+ check(Literal),!,
	List=[Literal|AccList].
clause_to_list(Literal v Literals,AccList,List):-
	clause_to_list(Literals,[Literal|AccList],List).

list_to_clause([],[]).
list_to_clause([Literal|Literals],Clause):-
	list_to_clause(Literals,Literal,Clause).
	
list_to_clause([],AccClause,AccClause).
list_to_clause([Literal|Literals],AccClause,Clause):-
	list_to_clause(Literals,Literal v AccClause,Clause).
	
% -------------------------------------------------------------------------------------------------
%                                Główny predykat rozwiązujący zadanie.
% -------------------------------------------------------------------------------------------------

prove([],_):-!,fail.
prove(Clauses, Proof):-
	organize(Clauses, NegativeTuples, RestTuples,Counter),
	resolutionList(RestTuples,NegativeTuples,Resolution),
	resolution(NegativeTuples,RestTuples,Resolution,Proof_tmp,Counter,[]),
	clear(Proof_tmp,Proof).

	
% =================================================================================================
%                                            Rezolucja
% =================================================================================================	

% -------------------------------------------------------------------------------------------------
% Początek listy wynikowej ...
% -------------------------------------------------------------------------------------------------
resolutionList([],[],[]).	
resolutionList([RestTuple|RestTuples],[],[RestTuple|X]):-
		resolutionList(RestTuples,[],X).	
resolutionList(RestTuples,[NegTuple|NegTuples],[NegTuple|X]):-
		resolutionList(RestTuples,NegTuples,X).	

	

% -------------------------------------------------------------------------------------------------
% Rezolucyjny dowód sprzeczności. Idea rozwiązania polega na wyprowadzaniu rezolwent 
% z przesłanek znajdujących się w zbiorze o negatywnym wystąpieniu zmiennych- "NegativeTuples" oraz
% zbiorze z resztą klauzul "RestTuples".Predykat wybiera pierwszą klauzule z "NegativeTuples" i tworzy 
% rezolwenty z klauzulami ze zbioru "RestTuples".  Powstałe rezolwenty są dołączane do odpowiadających 
% sobie zbiorów. W każdym kolejnym kroku dana klauzula z "NegativeTuples" jest zapamiętywana zapobiegając
% zapętleniu i odkładana. Program będzie próbował tworzyć nowe rezolwenty dopóki będzie istniała 
% klauzula w "NegativeTuples". 

% Uwaga: Listy klauzul jest posortowana względem długości klauzuli.
% Uwaga: Powstała rezolwenta nie może być tautologią. Także po każdej rezolwencie są usuwane nadzbiory.
%		 Jeśli istnieje rezolucyjny dowód sprzeczności to klauzula pusta będzie się znajdowała w 
%		 zbiorze klauzul o negatywnym wystąpieniu zmiennych. 
% -------------------------------------------------------------------------------------------------
	
resolution([],[],Proof,Proof,[],_):-!.
resolution([],_,_,_,_,_):-!,fail.
resolution([ClauseEmpty],[],Resolution,Resolution,_,_):-!,
	addFrame(_,_,0,_,ClauseEmpty).
resolution([NegTuple|NegTuples],RestTuples,Resolution,Proof,Counter,OldNeg):-
	resolution(NegTuple,RestTuples,[],NegResolvent,[],RestResolvent,Counter,NewCounter,OldNeg),
	order(RestTuples,RestResolvent,NegTuples,NegResolvent, Negative, Rest),
	proof(Resolution,Negative,NegTuples, Rest,RestTuples,NewResolution),
	( \+ is_finish(Negative) ->
	    resolution(Negative,Rest,NewResolution,Proof,NewCounter,[NegTuple|OldNeg])
	; Proof=NewResolution
	).

% -------------------------------------------------------------------------------------------------

resolution(_,[],AccNeg,AccNeg,AccRes,AccRes,Counter,Counter,_).
resolution(NegTuple,[RestTuple|RestTuples],AccNeg,NegResolvent,AccRes,RestResolvent,Counter,NewCounter,OldNeg):-
	NextCounter is Counter + 1,
	resolve_tuple(NegTuple,RestTuple,Resolvent,NextCounter,OldNeg),!,
	addFrame((Pos,_),_,Length,_,Resolvent),
	(
	 ( Length=0 ->
	    resolution(NegTuple,[],[Resolvent],NegResolvent,[],RestResolvent,[],_,_)
	 )
	;
	 ( Pos=[] ->
	    resolution(NegTuple,RestTuples,[Resolvent|AccNeg],NegResolvent,AccRes,RestResolvent,NextCounter,NewCounter,OldNeg)
	 ;  resolution(NegTuple,RestTuples,AccNeg,NegResolvent,[Resolvent|AccRes],RestResolvent,NextCounter,NewCounter,OldNeg)
	 )
	).
	
resolution(NegTuple,[_|RestTuples],AccNeg,NegResolvent,AccRes,RestResolvent,Counter,NewCounter,OldNeg):-
	resolution(NegTuple,RestTuples,AccNeg,NegResolvent,AccRes,RestResolvent,Counter,NewCounter,OldNeg).

% Sprawdzenie czy rezolwentą jest klauzula pusta
is_finish([Empty]):-
	addFrame((_,_),_,0,_,Empty).

% Porządkowanie zbiorów. Usuwanie nadzbiorów oraz sortowanie według długości. 	
order(RestTuples,RestResolvent,NegTuples,NegativeResolvent,SortNegative, SortRest):-
	append(RestTuples,RestResolvent,NewRest),
	append(NegTuples,NegativeResolvent,NewNeg),	
	append(NewNeg,NewRest,Tuples),
	sub(Tuples,NewTuples),
	divide(NewTuples,[],Negative,[],Rest),
	keysort(Negative, SortNegative),
	keysort(Rest, SortRest).	
		
% Dodawanie wyników rezolucji do listy wyjściowej.
proof(Resolution,Neg,SubNeg,Rest,SubRest,NewResolution):-
	subtract(Neg,SubNeg,JoinNeg),
	subtract(Rest,SubRest,JoinRest),	
	append(JoinRest,Resolution,Tmp),
	append(JoinNeg,Tmp,NewResolution).
	
	
% =================================================================================================
%                                    Czyszczenie listy wynikowej
% =================================================================================================

% -------------------------------------------------------------------------------------------------
% Czyszczenie listy wynikowej z nadmiarowych klauzul. Na początu zostaną wybrane tylko te klauzule 
% które przyczyniły się do sprzeczności a następnie zostaną odpowiednio ponumerowane. 
% -------------------------------------------------------------------------------------------------
clear(Tuples,Proof):-
	del(Tuples,Proof1),	
	correct(Proof1,Proof,0,[]).


% -------------------------------------------------------------------------------------------------
% Poprawienie numeracji w liście.
% -------------------------------------------------------------------------------------------------
 
correct([],[],_,_).
correct([Tuple|Tuples],[Resolvent|Proof],Counter,NumberList):-
	addFrame((Pos,Neg),Num,_,(Var,X,Y),Tuple),!,
	NextCounter is Counter +1, 
	position(X,NumberList,PosX),
	position(Y,NumberList,PosY),
	list_to_Clause(Pos,Neg,Clause),
	addOrigin(Clause,(Var,PosX,PosY),Resolvent),
	correct(Tuples, Proof, NextCounter,[Num-NextCounter|NumberList]).
correct([Tuple|Tuples],[Resolvent|Proof],Counter,NumberList):-
	addFrame((Pos,Neg),Num,_,axiom,Tuple),
	NextCounter is Counter +1, 
	list_to_Clause(Pos,Neg,Clause),
	addOrigin(Clause,Resolvent),
	correct(Tuples, Proof, NextCounter,[Num-NextCounter|NumberList]).
	
% -------------------------------------------------------------------------------------------------
% Usuwanie niepotrzebnych klauzul. Wybierane są tylko te, ktore przyczyniły się do sprzeczności.
% Program zaczyna od ostatniej klauzuli czyli klauzuli pustej.
% -------------------------------------------------------------------------------------------------
del([Tuple|Tuples],Proof):-
	( addFrame((_,_),_,_,(_,X,Y),Tuple) ->
		del(Tuples,[Tuple],Proof,[X,Y])
	;
	  addFrame((_,_),_,_,axiom,Tuple),
	  Proof=[Tuple]
	).


del([],Proof,Proof,_).
	
del([Tuple|Tuples],AccProof,Proof,NumberList):-
	addFrame((_,_),Num,_,(_,X,Y),Tuple),!,
	( memberchk(Num,NumberList) ->
	   ( delete(NumberList,Num,DelNumberList),
	     del(Tuples,[Tuple|AccProof],Proof,[X,Y|DelNumberList])
	   )
	; del(Tuples,AccProof,Proof,NumberList)
	).
del([Tuple|Tuples],AccProof,Proof,NumberList):-
	addFrame((_,_),Num,_,axiom,Tuple),
	( memberchk(Num,NumberList) ->
	   ( delete(NumberList,Num,DelNumberList),
		 del(Tuples,[Tuple|AccProof],Proof,DelNumberList)     
	   )
	;  del(Tuples,AccProof,Proof,NumberList)
	).	


% Sprawdzenie czy pozycja klauzuli się zmieniła po usunięciu niepotrzebnych klauzul z listy wynikowej. 
% "X" jest pozycją przesłanki, a NumberList listą par pozycji zmienionych (Stara pozycja-Nowa pozycja).

position(X,NumberList,Pos):-
	memberchk(X-Pos,NumberList),!.
position(X,_,X).		

% Przemianowanie z list zmiennych w klauzulę	

list_to_Clause([],[],[]):-!.
list_to_Clause([],[HNeg|TNeg],Clause):-
	list_to_Clause([],TNeg,~HNeg,Clause).
list_to_Clause([HPos|TPos],Neg,Clause):-
	list_to_Clause(TPos,Neg,HPos,Clause).

list_to_Clause([],[],AccClause,AccClause):-!.
list_to_Clause([],[HNeg|TNeg],AccClause,Clause):-
	list_to_Clause([],TNeg,~HNeg v AccClause,Clause).
list_to_Clause([HPos|TPos],Neg,AccClause,Clause):-
	list_to_Clause(TPos,Neg,HPos v AccClause,Clause).
	
% =================================================================================================
%                                    Produkcja Rezolwenty
% =================================================================================================
resolve_tuple(NegTuple,ResTuple,Resolvent,Counter,OldNegative):-
	select(NegTuple,ResTuple,Neg,Pos,Var,NumA,NumB),
	length(Neg,NewLength1),
	length(Pos,NewLength2),
	Length is NewLength1+NewLength2,
	addFrame((Pos,Neg),Counter,Length,(Var,NumB,NumA),Resolvent),
	suppression((Pos,Neg),[NegTuple,ResTuple|OldNegative]).
	
% Sprawdzamy czy zbiór jest równy lub jest nadzbiorem innych zbiorów.
suppression(_,[]).
suppression((Pos,Neg),[HTuple|TTuples]):-
	addFrame((HPos,HNeg),_,_,_,HTuple),
	(\+ subset(HPos,Pos) ; \+ subset(HNeg,Neg)),
	suppression((Pos,Neg),TTuples).
	
% Tworzenie rezolwenty. Wybierana jest jedna zmienna z której można utworzyć rezolwente.
% Jeśli ich jest kilka to, w efekcie zawsze otrzymamy tautologie. 
select(NegTuple,ResTuple,Neg,Pos,Var,NumA,NumB):-
	addFrame((_,NegA),NumA,_,_,NegTuple),
	addFrame((PosB,NegB),NumB,_,_,ResTuple),
	intersection(NegA,PosB,[Var]),
	delete(NegA,Var,DelNegA),
	delete(PosB,Var,Pos),
	merge_set(DelNegA, NegB, Neg).
	

% =================================================================================================
%                                Porządkowanie listy wejściowej
% =================================================================================================

% -------------------------------------------------------------------------------------------------
% Uporządkowanie klazul podanych na wejściu. Usunięte zostaną tautologie, nadzbiory oraz klauzule 
% które zawierają zbędne literały. Klauzula będzie miała nową reprezentację zwaną "Tuple".
% Reprezentacja: długość-((Zmienne Pozytywne,Zmienne Negatywne), Pozycja w liście, Pochodzenie klauzuli)
% 
% 
% Ważne: Klauzule są dzielone na takie w których występują TYLKO negatywne wystąpienia zmiennych oraz
%        reszte klauzul. Oby dwie są posortowane względem długości klauzul.
% -------------------------------------------------------------------------------------------------

organize(Clauses, NegativeTuples, RestTuples, Counter):-
	deleteClauses(Clauses,CleanClauses),
	presentation(CleanClauses,Tuples,0,Counter),
	sub(Tuples,CleanTuples),
	keysort(CleanTuples, SortedTuples),
	divide(SortedTuples,[],NegativeTuples,[],RestTuples).


% -------------------------------------------------------------------------------------------------
% Usunięte zostają klauzule, które zawierają zbędne zmienne, takie że nie posiadają oby dwóch 
% wystąpień: pozytywne i negatywne ( w wszystkich klauzulach) oraz tautologie. 
% -------------------------------------------------------------------------------------------------
deleteClauses(Clauses, CleanClauses):-
	literals(Clauses,ListClauses,[],PositiveLiterals,[],NegativeLiterals),
	unnecessary(PositiveLiterals,NegativeLiterals,Literals),
	reduce(ListClauses,CleanClauses,Literals).

% Usunięcie klauzul z tautologią i wybranie zmiennych pozytywnych oraz negatywnych. 
% Przemiana reprezentacji klauzul. Klauzule będą miały postać listy zmiennych o pozytywnym 
% wystąpieniu oraz o negatywnym wystąpieniu. 
literals([],[],Positive,Positive,Negative,Negative).
literals([[]|_],[([],[])],_,[],_,[]):-!.
literals([Clause|Clauses], [(SortPos,SortNeg)|ListClauses], AccPos, Positive, AccNeg, Negative):-
	\+ excluded_middle(Clause),!,
	selectLiteral(Clause,[],Pos,[],Neg),
	sort(Pos, SortPos),
    sort(Neg, SortNeg),
    merge_set(AccPos,SortPos,NewAccPos),
    merge_set(AccNeg,SortNeg,NewAccNeg),
    literals(Clauses, ListClauses, NewAccPos, Positive, NewAccNeg, Negative).
literals([_|Clauses], ListClauses, AccPos, Positive, AccNeg, Negative):-
	literals(Clauses, ListClauses, AccPos, Positive, AccNeg, Negative).

% Wybieranie zmiennych z klauzuli
selectLiteral([],AccPos,AccPos,AccNeg,AccNeg):-!.
selectLiteral(H,AccPos,Positive,AccNeg,Negative):-
	\+ check(H),!,	
	( negative(H) ->
	   (choose(H,X),
	    selectLiteral([],AccPos,Positive,[X|AccNeg],Negative)
	   )
	; selectLiteral([],[H|AccPos],Positive,AccNeg,Negative)
	).	
selectLiteral(H v T,AccPos,Positive,AccNeg,Negative):-
	( negative(H) ->
	   (choose(H,X),
	    selectLiteral(T,AccPos,Positive,[X|AccNeg],Negative)
	   )
	; selectLiteral(T,[H|AccPos],Positive,AccNeg,Negative)
	).

% Wybieranie niepotrzebnych zmiennych
unnecessary(Positive, Negative, Literals):-
	merge_set(Positive,Negative,MergeList),
	intersection(Positive, Negative, Intersection),
	subtract(MergeList,Intersection,Literals).

% Usuwanie klauzul które posiadają niepotrzebne zmienne
reduce([],[],_).
reduce([(Pos,Neg)|Clauses],[(Pos,Neg)|NewListClauses],Literals):-
	find(Pos,Neg,Literals),!,
	reduce(Clauses,NewListClauses,Literals).
reduce([_|Clauses],NewListClauses,Literals):-
	reduce(Clauses,NewListClauses,Literals).
	
% Sprawdzenie czy zmienne występują w klauzuli
find(Pos,Neg,Literals):-
	intersection(Pos,Literals,[]),
	intersection(Neg,Literals,[]).

% -------------------------------------------------------------------------------------------------
% Eliminiacja nadzbiorów
% -------------------------------------------------------------------------------------------------
sub([],[]). 
sub([Tuple|Tuples],[Tuple|NewTuples]):-	
	addFrame((Pos,Neg),_,_,_,Tuple),	
	sub((Pos,Neg),Tuples,[],Result),!, 
	sub(Result,NewTuples).
sub([_|Tuples],NewTuples):-		   
	sub(Tuples,NewTuples).
	
% Eliminacja nadzbiorów względem jedengo zbioru. Także sprawdzamy czy zbiór względem którego eliminujemy 
% inne zbiory także nie jest nadzbiorem.
sub(_,[],AccTuple,AccTuple).
sub((Pos,Neg),[HTuple|TTuples],AccTuple,NewTuples):-
	addFrame((HPos,HNeg),_,_,_,HTuple),
	( ( subset(Pos,HPos) , subset(Neg,HNeg) ) ->
	    (!,sub((Pos,Neg),TTuples,AccTuple,NewTuples)
	    )
	; ( \+ subset(HPos,Pos) ; \+ subset(HNeg,Neg) ) ->
	    sub((Pos,Neg),TTuples,[HTuple|AccTuple],NewTuples)
	).
% -------------------------------------------------------------------------------------------------	
% Zamiana klauzul na krotki.  
% Reprezentacja: długość-((Zmienne Pozytywne,Zmienne Negatywne), Pozycja w liście, Pochodzenie klauzuli)	
% -------------------------------------------------------------------------------------------------
presentation([],[],Counter,Counter).
presentation([(Pos,Neg)|Clauses],[Tuple|ListTuples],Counter,NewCounter):-
	NextCounter is Counter+1,
	length(Pos,Len1),
	length(Neg,Len2),
	Length is Len1+Len2,
	addFrame((Pos,Neg),NextCounter,Length,axiom,Tuple),
	presentation(Clauses,ListTuples,NextCounter,NewCounter).

% -------------------------------------------------------------------------------------------------	
% Dzielenie zbioru na 2 części: Zbiór klauzul z wystąpieniem negatywnych zmiennych oraz reszta klauzul. 
% -------------------------------------------------------------------------------------------------
divide([],NegAcc,NegAcc,RestAcc,RestAcc).
divide([Tuple|Tuples],NegAcc,NegativeTuples,RestAcc,RestTuples):-
	addFrame((Pos,_),_,_,_,Tuple),
	( Pos=[] ->
	   divide(Tuples,[Tuple|NegAcc],NegativeTuples,RestAcc,RestTuples)
	; divide(Tuples,NegAcc,NegativeTuples,[Tuple|RestAcc],RestTuples)
	).	
	
% Sprawdzenie czy zmienne mają negatywne wystąpienie w klauzuli 
is_negative(~H):- 
	\+ check(H),!.
is_negative(~_ v T):-
	is_negative(T).

% Sprawdzenie czy klauzula podlega prawu wyłączonego środka
excluded_middle(H v T):-
	run(H,T),!.
excluded_middle(_ v T):-
	excluded_middle(T).	
run(H,~H):-
	\+ check(H),!.
run(~H,H):-
	\+ check(H),!.
run(H,~H v _):-!.
run(~H,H v _):-!.
run(H, _ v T):-
	run(H,T).

% =================================================================================================
%                                            Podstawowe
% =================================================================================================

% Reprezentacja klauzuli 
addFrame((Pos,Neg),Num,Length,axiom,Length-((Pos,Neg),Num,axiom)):-!.
addFrame((Pos,Neg),Num,Length,(Var,X,Y),Length-((Pos,Neg),Num,(Var,X,Y))).
addOrigin(Clause,(Clause, axiom)):-!.
addOrigin(Clause,(Var,PosX,PosY),(Clause,(Var,PosX,PosY))).

% Wybieranie zmiennej z literału
choose(~X,X):- !.
choose(X,X).

% Predykat sprawdza czy zmienna ma negatywne wystąpienie
negative(~_).

% Predykat sprawdza czy klauzula jest złożona
check(_ v _).
