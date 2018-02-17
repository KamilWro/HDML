:- module(kamil_breczko_parser, [parse/3]).

% ==================================================================================================
%											 UWAGA
%
% WERSJA 1		Wersja przed testami
% Testy uznaja za poprawne komentarze takie, ze komentarz zaczyna sie od sekwencji znakow "(*" 
% a kończy sie "*)", a pomiedzy nimi moze znajdowac sie dowolna sekwencja znakow, nie 
% zawierajaca "*)", ktora nie  zamyka zagniezdzonego komentarza. Przy czym dozwolone jest 
% dowolna ilosc sekwencji znakow "(*" ktora nie koniecznie musi otwierac nowy komentarz, tzn. moze byc
% traktowana jako znaki pod komentarzem. Podczas analizy, algorytm przyjmuje zasade zachlonnosci. 
% Testy ktore sprawdzaja poprawnosc takich komentarzy to: empty, comment. 
%
% WERSJA 2		Wersja po testach
% Komentarz uznamy za prawidlowy, wtw gdy komentarz  zaczyna sie od sekwencji znakow "(*" 
% a kończy sie "*)", a pomiedzy nimi:
% 	- moze znajdowac sie dowolna sekwencja znakow, 
% 	- nie moze znajdowac sie sekwencja znakow zawierajaca "*)", ktora nie  zamyka zagniezdzonego komentarza 
%	- nie moze znajdowac sie sekwencja znakow zawierajaca "(*", ktora nie  otwiera zagniezdzonego komentarza 
%
%
%
% ==================================================================================================

parse(Path, Codes, Program) :-
	( var(Path) ->
	   Path=test
	; true
	),
	Pos=..[file,Path,1,1,0,_],
	phrase(lexer(TokList,Pos), Codes),
    phrase(program(Program), TokList).

% ==================================================================================================
%											Analizator leksykalny
% ==================================================================================================

lexer(Tokens,Pos) -->
   white_space(Pos,TmpPos),
   (
    "(*", 			comments(TmpPos,NewPos),!, lexer(Tokens,NewPos) 
   ;
    ( (  "<>",  	!, { Token = tokNeq(TmpPos) } 
      ;  "..",		!, { Token = tokDoubleDot(TmpPos)}
      ;  "<=",  	!, { Token = tokLeq(TmpPos) }
      ;  ">=",  	!, { Token = tokGeq(TmpPos) }
      ), {next_num(TmpPos,NewPos,2)}
    ;
      (  "(",		!, { Token = tokLParen(TmpPos) }
      ;  ")",   	!, { Token = tokRParen(TmpPos) }
      ;  "[",		!, { Token = tokLSquare(TmpPos) }
      ;  "]",		!, { Token = tokRSquare(TmpPos) }
      ;  ",",		!, { Token = tokComma(TmpPos)}
      ;  "=",   	!, { Token = tokAssgn(TmpPos) }
      ;  "<",   	!, { Token = tokLt(TmpPos) }        
      ;  ">",   	!, { Token = tokGt(TmpPos) }  
      ;  "^",   	!, { Token = tokXOR(TmpPos) }
      ;  "|",   	!, { Token = tokOR(TmpPos) }
      ;  "&",   	!, { Token = tokAND(TmpPos) }
      ;  "+",   	!, { Token = tokPlus(TmpPos) }      
      ;  "-",   	!, { Token = tokMinus(TmpPos)  }
      ;  "*",   	!, { Token = tokTimes(TmpPos) }
      ;  "/",   	!, { Token = tokDiv(TmpPos) }
      ;  "@",   	!, { Token = tokAt(TmpPos) }
	  ;  "%",		!, { Token = tokMod(TmpPos) }
	  ;  "#",		!, { Token = tokHash(TmpPos)  }
	  ;  "~",		!, { Token = tokNOT(TmpPos)  }
	  ), {next_num(TmpPos,NewPos,1)}	  	        
    ; numbers(TmpPos,NewPos,Token), !
	; id(TmpPos,NewPos,Token),	!
    ;  [_], 		!, { getFile(TmpPos,NewPos), throwLexer("invalid atom format", NewPos) }
    ),				!, { Tokens = [Token | TokList] }, lexer(TokList,NewPos)
   ;   [], { Tokens = [] }
   ).

% --------------------------------------------------------------------------------------------------
%										  Biale znaki
% --------------------------------------------------------------------------------------------------
white_space(Pos,NewPos) -->
	[Char], { code_type(Char, space)} , !, next_char(Pos,TmpPos,Char), white_space(TmpPos,NewPos).
white_space(Pos,Pos) -->
	[].

% --------------------------------------------------------------------------------------------------
%											Liczba
% --------------------------------------------------------------------------------------------------
numbers(Pos,NewPos, Token)-->
	digit(D), number(D, N),{ size(N,S), next_num(Pos,NewPos,S),  Token = tokNumber(N,Pos) }.


digit(D) -->
	[D],{ code_type(D, digit) }.

digits([D|T]) -->
	digit(D),!,digits(T).
digits([]) -->
	[].

number(D, N) -->
	digits(Ds),{ number_chars(N, [D|Ds])}.

% --------------------------------------------------------------------------------------------------
%										 Identyfikator
% --------------------------------------------------------------------------------------------------
id(Pos,NewPos,Token)-->
	letter(L), identifier(L, Id), {size(Id,S), next_num(Pos,NewPos,S)},
    { member((Id, Token), [
    	(def, tokDef(Pos)),
    	(else, tokElse(Pos)),
      	(if, tokIf(Pos)),
      	(in, tokIn(Pos)),
      	(let,tokLet(Pos)),
      	(then, tokThen(Pos)),
      	('_',tokLine(Pos))]),!
	; Token = tokVar(Id,Pos)
    }.


letter(L) -->
	[L], 
	( { code_type(L, alpha) },!
	; { atom_codes("_",[L])}
	).

alphanum([A|T]) -->
	[A],
	( { code_type(A, alnum) }
	; { atom_codes("_",[A])}
	; { atom_codes("'",[A])}
	),!, alphanum(T).
alphanum([]) -->
	[].

identifier(L, Id) -->
	alphanum(As),{ atom_codes(Id, [L|As]) }.


% --------------------------------------------------------------------------------------------------
%										 Komentarz
% --------------------------------------------------------------------------------------------------	

% Wersja 1 	(zakomentowana, poniewaz sprawdzaczka uznaje za prawidlowa wersje 2)
% comments(Acc,Pos) -->
% 	comment(Acc,Pos).
% comments(Pos,Pos) -->
%	[],{throwLexer("unterminated comment", Pos)}.
% comment(Acc,Pos) -->
%	"*)",{next_num(Acc,Pos,4)},!.   
% comment(Acc,Pos) -->
%	"(*", comment(Acc,NewAcc),
%	comment(NewAcc,Pos). 
% comment(Acc,Pos) -->	
% 	[ASCII], next_char(Acc,NewAcc,ASCII), comment(NewAcc,Pos).

% Wersja 2 
% (roznica: odciecie ktora uniemozliwia nawrotu i potraktowanie sekwencji znakow "(*" jako komentarz)

 comments(Acc,Pos) -->
 	comment(Acc,Pos).
 comments(Pos,Pos) -->
	[],{throwLexer("unterminated comment", Pos)}.
 comment(Acc,Pos) -->
	"*)",!,{next_num(Acc,Pos,4)}.   
 comment(Acc,Pos) -->
	"(*",!, comment(Acc,NewAcc),
	comment(NewAcc,Pos). 
 comment(Acc,Pos) -->	
 	[ASCII], next_char(Acc,NewAcc,ASCII), comment(NewAcc,Pos).
	
% --------------------------------------------------------------------------------------------------
%										Pomocnicze do lexer/2
% --------------------------------------------------------------------------------------------------
next_char(file(P, L, LP, CN, _), file(P, L_, LP_, CN_, _), ASCII)-->
	( next(Char),
	    { (ASCII=13, Char=10) -> 	% znak nowego wiersza [ Windows ]
			(L_ is L+1, LP_=1, CN_ is CN+2)
		}
	; {ASCII=10 -> 				% znak nowego wiersza [ UNIX ]
		(L_ is L+1, LP_=1, CN_ is CN+1)
	  }
	; {ASCII=13 -> 				% znak nowego wiersza [ MAC OS ]
		(L_ is L+1, LP_=1, CN_ is CN+1)
	  }
	; {ASCII=11 -> 				% znak tabulacji pionowej
		(L_ is L+1, LP_=1, CN_ is CN+1)
	  }
	; {ASCII=12 -> 				% znak nowej strony
		(L_ is L+1, LP_=1, CN_ is CN+1)
	  }
	; {							% kolejny dowolny znak
	  	(L_= L, LP_ is LP+1, CN_ is CN+1)
	  }
	).	

next(Char)-->
	[Char],{Char=10},!.
next([])-->
	[].

	
next_num(file(P, L, LP, CN, Num), file(P, L, LP_, CN_, _), Num):-
	LP_ is LP+Num, 
	CN_ is CN+Num.

size(Id,S):-	
	atom_codes(Id, As),
	length(As,S).	
	
throwLexer(Reason,Pos):-
	throw(syntax_error(Reason, Pos)).
	
% ==================================================================================================
%										Analizator skladniowy
% ==================================================================================================
% W celu okreslenia dokladnej pozycji program korzysta z podanego oznaczenia:
% 	PosA(Accumulator Position)- Akumulator z pozycja
% 	PosB(Begin Position)- Pozycja "Od"
% 	PosE(End Position)- Pozycja "Do"
% 	PosL(Last Position)- Pozycja ostatniego wczytanego znaku
% Dokladna pozycja wyrazenia jest wyznaczana przez PosB, a dlugosc to roznica miedzy PosE a PosB oraz
% suma z dlugoscia wyrazenia PosE (wyznaczana przez predykat getPos).

% Reprezentacja pozycji w kodzie:
% file(Path, Line, LinePos, CharNo, Length) 
%  gdzie Path oznacza sciezke do pliku 
%        Line jest numerem wiersza (liczac od 1), 
%        LinePos jest numerem znaku w wierszu (liczac od 1), 
%		 CharNo jest numerem znaku w pliku (liczac od 0), 
%		 Length jest dlugoscia wskazywanego miejsca.

% Dodatkowa reprezentacja korzystana przy wyjatkach:
% file(Path, Line, LinePos, CharNo) 
%  Podobnie jak wyzej, ale bez podawania dlugosci.


% --------------------------------------------------------------------------------------------------
%											Program
% --------------------------------------------------------------------------------------------------
program(Program) -->
	definitions(Program).
program([]) -->
	[].

% --------------------------------------------------------------------------------------------------
%											Definicje
% --------------------------------------------------------------------------------------------------	
definitions(Program)-->
	definition(Def),!, {Program=[Def|Rest]}, definitions(Rest).
definitions([])-->
	[].
definitions(_)-->
	throwParser("incorrect definition").
	
definition(def(Name,Pat,Exp))-->
	[tokDef(_)],[tokVar(Name,_)],
	[tokLParen(_)],patterns(Pat,_,_),[tokRParen(_)],
	[tokAssgn(_)],!,expression(Exp,_,_).
definition(_)-->
	throwParser("incorrect definition").
	
% --------------------------------------------------------------------------------------------------
%											Wzorzec
% --------------------------------------------------------------------------------------------------
patterns(Pat,PosB,PosE)-->
	  patterns_lvl(X,PosB,PosA),
	  ( [tokComma(_)],!,patterns(Y,_,PosE),
	    {getPos(PosB,PosE,Pos), Pat=.. [pair,Pos,X,Y]}
	  ; {Pat= X,
	     PosE=PosA
	    }
	  ).
patterns(_,_,_)-->
	throwParser("incorrect pattern").	  
	
patterns_lvl(Pat,PosB,PosE)-->
	  [tokLParen(PosB)],!,patterns(Pat,_,_),[tokRParen(PosE)]
	; pattern(Pat,PosB,PosE).
	
pattern(Pat,Pos,Pos)-->
	( [tokLine(Pos)],!,{ Pat =.. [wildcard,Pos]}
	; [tokVar(X,Pos)],!,{ Pat =.. [var,Pos,X]}
	; throwParser("invalid argument in pattern")
	).
	
% --------------------------------------------------------------------------------------------------
%											Wyrazenie
% --------------------------------------------------------------------------------------------------
expression(Exp,PosB,PosE)-->
	[tokIf(PosB)],!,expression(ExpIf,_,_),
	[tokThen(_)],!,expression(ExpThen,_,_),
	[tokElse(_)],!,expression(ExpElse,_,PosE),
	{getPos(PosB,PosE,Pos), Exp=..[if,Pos,ExpIf,ExpThen,ExpElse]}.
expression(Exp,PosB,PosE)-->
	[tokLet(PosB)],!,patterns(Pat,_,_),
	[tokAssgn(_)],!,expression(ExpLet,_,_),
	[tokIn(_)],!,expression(ExpIn,_,PosE),
	{getPos(PosB,PosE,Pos), Exp=..[let,Pos,Pat,ExpLet,ExpIn]}.
expression(Exp,PosB,PosE)-->
	expression_op_1(Exp,PosB,PosE).
expression(_,_,_)-->
	 throwParser("incorrect expression").

% --------------------------------------------------------------------------------------------------
%								Wyrazenie z operatorem binarnym
% --------------------------------------------------------------------------------------------------

% op_1= [ ',' ]
expression_op_1(Exp,PosB,PosE)-->
	expression_op_2(Exp1,PosB,PosA),
	( op_1(_,_),!,expression_op_1(Exp2,_,PosE),
	  {getPos(PosB,PosE,Pos),Exp=..[pair,Pos,Exp1,Exp2]}
	; {Exp=Exp1,PosE=PosA}
	).

% op_2=['=','<>','<','>','<=','>=']
expression_op_2(Exp,PosB,PosE)-->
	expression_op_3(Exp1,PosB,PosA),
	( op_2(X,_),!,expression_op_3(Exp2,_,PosE),
	  {getPos(PosB,PosE,Pos),Exp=..[op,Pos,X,Exp1,Exp2]}
	; {Exp1=Exp,PosE=PosA}
	).
%  op_3=[ '@' ]	
expression_op_3(Exp,PosB,PosE)-->
	expression_op_4(Exp1,PosB,PosA),
	( op_3(X,_),!,expression_op_3(Exp2,_,PosE),
	  {getPos(PosB,PosE,Pos),Exp=..[op,Pos,X,Exp1,Exp2]}
	; {Exp1=Exp,PosE=PosA}
	).

% op_4=['|','^','+','-']
expression_op_4(Exp,PosB,PosE)-->
	expression_op_5(Acc,PosB,PosA),expression_op_4(Acc,Exp,PosB,PosE,PosA).
expression_op_4(Acc,Exp,PosB,PosE,_)-->
	op_4(X,_),!,expression_op_5(Exp1,_,PosA),
	{getPos(PosB,PosA,Pos), Acc1=..[op,Pos,X,Acc,Exp1]},
	expression_op_4(Acc1,Exp,PosB,PosE,PosA).
expression_op_4(Exp,Exp,_,PosE,PosE)-->
	[].

% op_5=[ '&','*','/','%'] 
expression_op_5(Exp,PosB,PosE)-->
	expression_op_unary(Acc,PosB,PosA),expression_op_5(Acc,Exp,PosB,PosE,PosA).
expression_op_5(Acc,Exp,PosB,PosE,_)-->
	op_5(X,_),!,expression_op_unary(Exp1,_,PosA),
	{getPos(PosB,PosA,Pos), Acc1=..[op,Pos,X,Acc,Exp1]},
	expression_op_5(Acc1,Exp,PosB,PosE,PosA).
expression_op_5(Exp,Exp,_,PosE,PosE)-->
	[].

% --------------------------------------------------------------------------------------------------
%								Wyrazenie z operatorem unarnym
% --------------------------------------------------------------------------------------------------

% op_unary= ['-','#','~']
expression_op_unary(Exp,PosB,PosE)-->
	op_unary(X,PosB),!,expression_op_unary(Exp1,_,PosE),
	{ getPos(PosB,PosE,Pos),Exp=..[op,Pos,X,Exp1]}.
expression_op_unary(Exp,PosB,PosE)-->
	choice_bit(Exp,PosB,PosE).
		
% --------------------------------------------------------------------------------------------------
%										Wybor bitow
% --------------------------------------------------------------------------------------------------
choice_bit(Exp,Acc,PosB,PosE,_)-->
	[tokLSquare(_)],!,expression(Exp2,_,_),
	( [tokRSquare(PosA)],!, 
	  {getPos(PosB,PosA,Pos), Acc2=..[bitsel,Pos,Acc,Exp2]}
	; [tokDoubleDot(_)],!, expression(Exp3,_,_),[tokRSquare(PosA)], 
	  {getPos(PosB,PosA,Pos), Acc2=..[bitsel,Pos,Acc,Exp2,Exp3]}
	; throwParser("incorrect choice bits")
	),choice_bit(Exp,Acc2,PosB,PosE,PosA).
choice_bit(Exp,Exp,_,PosE,PosE)-->
	[].	
choice_bit(Exp,PosB,PosE)-->
	expression_simple(Acc,PosB,PosA),choice_bit(Exp,Acc,PosB,PosE,PosA).

% --------------------------------------------------------------------------------------------------
%									 Wyrazenie proste
% --------------------------------------------------------------------------------------------------
expression_simple(Exp,PosB,PosE)-->
	  [tokLParen(PosB)],!,expression(Exp,_,_),[tokRParen(PosE)]
	; expression_atomic(Exp,PosB,PosE).

% --------------------------------------------------------------------------------------------------
%								 	Wyrazenie atomowe
% --------------------------------------------------------------------------------------------------
expression_atomic(Exp,PosB,PosE)-->
	  call_function(Exp,PosB,PosE),!
	; variable(Exp,PosB,PosE),!  
	; num(Exp,PosB,PosE),!
	; vector(Exp,PosB,PosE),!
	; throwParser("expected atom").

num(Exp,Pos,Pos)-->
	[tokNumber(N,Pos)],{Exp=..[num,Pos,N]}.
variable(Exp,Pos,Pos)-->
	[tokVar(X,Pos)],{Exp=..[var,Pos,X]}.
call_function(Exp,PosB,PosE)-->
	[tokVar(Name,PosB)],[tokLParen(_)],
	( expression(Exp1,_,_),[tokRParen(PosE)]
	; throwParser("incorrect call function")
	),{getPos(PosB,PosE,Pos), Exp=..[call,Pos,Name,Exp1]}.

vector(Exp,PosB,PosE)-->
	[tokLSquare(PosB)],
	( [tokRSquare(PosE)],!,
	  {getPos(PosB,PosE,Pos), Exp=..[empty,Pos]}
	; expression(Exp1,_,_),[tokRSquare(PosE)],!,
	  {getPos(PosB,PosE,Pos), Exp=..[bit,Pos,Exp1]}
	; throwParser("Incorrect vector")
	).
	
% --------------------------------------------------------------------------------------------------
%									Operatory binarne i unarne
% --------------------------------------------------------------------------------------------------	
op_1(',',Pos)-->	
	[tokComma(Pos)].
op_2('=',Pos)-->
	[tokAssgn(Pos)],!.
op_2('<>',Pos)-->
	[tokNeq(Pos)],	!.
op_2('<',Pos)-->
	[tokLt(Pos)],	!.
op_2('>',Pos)-->
	[tokGt(Pos)],	!.
op_2('<=',Pos)-->
	[tokLeq(Pos)],	!.
op_2('>=',Pos)-->
	[tokGeq(Pos)].		
op_3('@',Pos)-->
	[tokAt(Pos)].
op_4('|',Pos)-->
	[tokOR(Pos)],	!.
op_4('^',Pos)-->
	[tokXOR(Pos)],	!.
op_4('+',Pos)-->
	[tokPlus(Pos)],	!.
op_4('-',Pos)-->
	[tokMinus(Pos)].
op_5('&',Pos)-->
	[tokAND(Pos)],	!.
op_5('*',Pos)-->
	[tokTimes(Pos)],!.
op_5('/',Pos)-->
	[tokDiv(Pos)],	!.
op_5('%',Pos)-->
	[tokMod(Pos)].
op_unary('#',Pos)-->
	 [tokHash(Pos)],!.	
op_unary('~',Pos)-->
	[tokNOT(Pos)],	!.
op_unary('-',Pos)-->
	[tokMinus(Pos)].
	
% ==================================================================================================
%											Pomocnicze
% ==================================================================================================		
	
% Zwraca pozycje wyrazenia wraz z dlugoscia wyrazenia
getPos(PosB,PosE,Pos):-
	 PosB=file(P, L, LP, CNB, _),
	 PosE=file(_, _, _, CNE, LenE),
	 Length is (CNE-CNB)+LenE,	   
	 Pos=file(P, L, LP, CNB, Length).	
	
% Wykorzystywane w zglaszaniu wyjatku. Zostanie usunieta dlugosc wyrazenia.
getFile(file(P, L, LP, CN, _),Pos):-
	Pos=file(P, L ,LP, CN).	
% Zglaszanie wyjatkow w Parser.	
throwParser(Reason)-->
	chooseToken(Pos),
	{getFile(Pos,PosThrow),throw(syntax_error(Reason, PosThrow))}.	

% Wybor nastepnego tokenu, ktory jest zle umieszczony w programie (oczekiwano innego). Uzywane przy
% wyrzucaniu wyjatkow w throwParser
chooseToken(Pos)-->
	  op_1(_,Pos),			! 
	; op_2(_,Pos),			!
	; op_3(_,Pos),			!
	; op_4(_,Pos),			!
	; op_5(_,Pos),		 	!
	; op_unary(_,Pos),		!
	; [tokDoubleDot(Pos)],	!
	; [tokLParen(Pos)],		!
	; [tokRParen(Pos)],		!
	; [tokLSquare(Pos)],	!
	; [tokRSquare(Pos)],	!
	; [tokNumber(_,Pos)],	!
	; [tokThen(Pos)],		!
	; [tokLine(Pos)],		!
	; [tokLet(Pos)],		!
	; [tokIn(Pos)],			!
	; [tokIf(Pos)],			!
	; [tokElse(Pos)],		!
	; [tokDef(Pos)],		!
	; [tokVar(_,Pos)].
