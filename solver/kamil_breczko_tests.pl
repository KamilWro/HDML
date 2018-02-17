% Definiujemy moduł zawierający testy.
:- module(kamil_breczko_tests, [tests/5]).

% definiujemy operatory ~/1 oraz v/2
:- op(200, fx, ~).
:- op(500, xfy, v).

% Zbiór faktów definiujących testy
% Należy zdefiniować swoje testy
% tests(-Name,-Type,-Input,-Timeout,-Ans))
%	Name: atom reprezentujący nazwę testu. Nazwa powinna mówić coś o teście i jednoznacznie go identyfikować.
%	Type: atom reprezentujący typ testu. Powinien przyjąć jedną z następujących wartości:
%		validity oznacza test poprawnościowy. 
%		performance oznacza test wydajnościowy. 
%	Input: zbiór klauzul będący danymi wejściowymi.
%	Timeout: liczba całkowita z przedziału 500–10000 oznaczająca limit czasowy dla
%		tego testu wyrażony w milisekundach.
%	Ans: term reprezentujący oczekiwaną odpowiedź. Powinien mieć jedną z następujących postaci:
%		solution(σ) gdzie σ jest wartościowaniem spełniającym Input;
%		count(n) gdzie n jest liczbą różnych wartościowań spełniających Input

%--------------------------------------------------------------------------------------------------
% 					Testy poprawnościowe
%--------------------------------------------------------------------------------------------------

% Proste
tests(single_1, validity, [ ~p ], 500, solution([(p,f)])).
tests(single_2, validity, [ p ], 500, solution([(p,t)])).
tests(no_clauses, validity, [ ], 500, solution([ ])).
tests(no_clauses_1, validity, [ ], 500, count(1)).
tests(one_clause_1, validity, [ p v q ], 500, solution([(p,t),(q,f) ])).
tests(one_clause_2, validity, [ p v ~q], 500, count(3)).
tests(one_clause_3, validity, [ ~p v ~q], 500, solution([(p,f),(q,f)])).
tests(one_long_clause, validity, [ p v q v p v q v s ], 500, solution([(p, f), (q, f), (s, t)])). 
tests(short_clauses, validity, [ ~p, ~p, ~q, r, ~q, r, ~p, r ], 500, solution([(p, f), (q, f), (r, t)])).
tests(only_negation, validity, [ ~p v ~q, ~q v ~s, ~s v ~t ], 500, solution([(p, f), (q, t), (s, f), (t, t)])).
tests(implication, validity, [ ~p, ~p v q ], 500, solution([(p, f), (q, t)])).

tests(generalized_valuation_1, validity, [ p, q v ~r ], 500, solution([(p, t),(q, t),(r, f)])).
tests(generalized_valuation_2, validity, [ p v q, p v ~q ], 500, count(2)).

tests(excluded_middle, validity, [ p v ~p, q v ~q, r v ~r ], 500, solution([(p, f), (r, f), (q, f)])).
tests(excluded_middle_extended, validity, [ p v q v ~p, q v ~q v p, ~q v ~p ], 
	500, solution([(p, t), (q, f)])).

tests(dependence_literals, validity, [ p, p v q, p v q v t], 500, solution([(p, t), (q, t), (t, t)])).
tests(dependence_literals_extended, validity, [ p v q, p v s v t, ~p ], 
	500, solution([(p, f), (q, t), (s, f), (t, t)])).

tests(repeated_literals_1, validity, [ p v q v p v q v s ], 500, count(7)).
tests(repeated_literals_2, validity, [ p v p, p v q ], 500, count(2)).
tests(repeated_clauses, validity, [ p v ~s, p v q v p v q v s, p v ~s, p v q v p v q v s],
	 500, solution([(p, t), (q, f), (s, f)])).

% Brak wartościowań
tests(clause_empty, validity, [~p, [], p v q], 500, count(0)).
tests(contradiction_1,validity,[p v q, q v ~p, p, ~q],500,count(0)).
tests(contradiction_2, validity, [p, ~p], 500, count(0)).

% Wszystkie wartościowania 
tests(first_of_three_valuations, validity, [ ~p v p v r, p v r v ~r, q v ~r v p , ~r v p v ~q, ~r v q ,
	p v r, p v ~p , p v ~r v r, p v ~q, ~q v p v ~p, ~r v r v q, ~r v r v p ],
	500, solution([(p, t), (r, f), (q, t)])).
tests(second_of_three_valuations, validity, [ ~p v p v r, p v r v ~r, q v ~r v p, ~r v p v ~q, ~r v q,
	p v r, p v ~p, p v ~r v r, p v ~q, ~q v p v ~p, ~r v r v q, ~r v r v p ], 
	500, solution([(p, t), (r, f), (q, f)])).
tests(third_of_three_valuations, validity, [~p v p v r, p v r v ~r, q v ~r v p, ~r v p v ~q, ~r v q,
	p v r, p v ~p, p v ~r v r, p v ~q, ~q v p v ~p, ~r v r v q, ~r v r v p ], 
	500, solution([(p, t), (r, t), (q, t)])).

% Jest możliwa tylko 1 odpowiedz
tests(one_answer, validity, [p, ~q, ~r v ~p], 500, count(1)).
tests(one_valuation, validity, [p, ~q, ~r v ~p], 500, solution([(p, t), (q, f), (r, f)])).
tests(one_answer_extended,validity,[~p, ~p v q, ~q v r, ~r v s, ~s],500,count(1)).
tests(one_valuation_extended,validity,[~p, ~p v q, ~q v r, ~r v s, ~s],500,solution([(p, f),(q, f),(r, f),(s, f)])).

% Skomplikowane
tests(tricky_0, validity, [ ~p v q v s, ~q v ~p v p, q v ~s v ~p, s v ~s v ~q , 
	~p v s v p, ~p v ~s v ~q ], 500, solution([(p, t), (s, f), (q, t)])).
tests(tricky_1, validity, [p v q v ~p, p v q v r, ~q], 500, solution([(p, f), (r, t), (q, f)])).
tests(tricky_2, validity, [p v q, r v ~q v ~p, ~q v r], 500, solution([(p, f), (q, t), (r, t)])).
tests(tricky_3, validity, [p, ~q, r v ~o, ~o v ~p], 500, solution([(o, f), (p, t), (q, f), (r, f)])).


% Klauzule złożone
tests(number_of_valuations1, validity, 
	[~r v r, q v r v p, ~p v w v q, ~p v p v ~w, ~w v ~p v q, q v ~r v ~q, p v ~r v ~p, ~r v r v q ],
	500, count(10)).
tests(number_of_valuations2, validity, [p v q v r v w, p v ~w, r v ~w , w v ~r v ~q, ~q v ~r],
	500, count(6)).
tests(number_of_valuations3, validity, [~q v q, p v ~r v ~p, p v q v r, r v p v ~q, ~p v r, p v ~r v ~q],
	500, count(3)).
tests(clauses_complex_1, validity, [p v q, ~p v r,~q v ~r], 500, solution([(p,f),(q,t),(r,f)])).
tests(clauses_complex_2, validity, [~p v p, ~r v ~q, r v p v q, p v ~q v ~r, p v ~r], 
	500, solution([(p,f),(q,t),(r,f)])).
tests(clauses_complex_3, validity, [p, p v q v r, ~q v r, r v t, ~q v t, ~t], 
	500, solution([(p, t), (q, f), (r, t), (t, f)])).

% Liczba wartościowań dla podanej klauzuli.
tests(many_valuations_1, validity, [p v q v r, p v q v s, s v t v g], 500, count(46)).
tests(one_of_many_valuations_1, validity, [p v q v r, p v q v s, s v t v g], 500, solution([ (g, t), 
	(p, t), (q, f), (r, f), (s, f), (t, t)] )).
tests(many_valuations_2, validity, [~p v q v s, ~s v t v r, ~r v p ], 500, count(16)).
tests(one_of_many_valuations_2, validity, [~p v q v s, ~s v t v r, ~r v p ], 500, solution([ (p, f),
	 (q, f), (r, f), (s, f), (t, f)] )).

%Prawa logiczne
tests(structural_dilemma, validity, [ r v p v q v ~p, r v ~r v q v ~p, r v p v ~r v ~p, r v ~r v ~r v ~p,
	 r v p v q v ~q, r v ~r v q v ~q, r v p v ~r v ~q, r v ~r v ~r v ~q ],
	 500, solution([(p,f),(q,f),(r,f)])).
tests(modus_pones, validity, [q v ~p v p, q v ~p v ~q], 500, solution([(p,t),(q,t)])).
tests(peirces_law, validity, [p v ~p v q, p v ~p], 500, count(4)).
tests(de_Morgans_law_negation, validity,
	 [~p v p v q, ~q v p v q, p v q v p v q, ~p v ~p, ~q v ~p, p v q v ~p, ~p v ~q, ~q v ~q, p v q v ~q ], 
	500, count(0)).
tests(modus_tollendo_tollens, validity, [~p v q v p, ~p v q v ~q], 500, solution([(p,t),(q,f)])).

%--------------------------------------------------------------------------------------------------
% 					Testy wydajnościowe
%--------------------------------------------------------------------------------------------------
tests(short_test, performance, 
[~zq v ~ut v ~uz, ~ut v zg v ~uw , ~zg v ~q v zu, zs v ~uw v o, ~o v ~zz v zs, ~zs v ~q v ~zu, 
~uo v ~zg v ~w, uo v ~ur v ~uq, ~uq v ~w v ~zq, zq v o v ~r, p v zs v og, zr v ~uu], 
700, solution([(uu, f), (og, t), (zs, t), (r, f), (o, t), (zq, t), (w, f), (uq, f), (ur, f), (zg, t), (zu, t),
(q, f), (zz, f), (uw, f), (ut, f), (uz, f), (uo, t), (p, t), (zr, t)])).

tests(many_variables, performance, 
[~zq v ~ut v ~uz , ~ut v zg v ~uw , ~zg v ~q v zu , zs v ~uw v o , ~o v ~zz v zs , 
~zs v ~q v ~zu , ~uo v ~zg v ~w , uo v ~ur v ~uq , ~u v ~ut v ~uq , r v zr v ~r , 
t v ~u v o , ~zq v zu v ~zr ,  ur v ~uo v uq , ~t v ~zu v ~q , u v ~u v og ,  zs v ~uo v s , 
zr v ~s v ~uz ,  zw v up v ~q], 
1000, solution([ (zq, t), (ut, t), (uz, f), (zg, f) ,(uw, f), (q, t), (zu, f), (zs, t), (o, t), (zz, t), (uo, t), 
(w, t), (ur, t), (uq, f), (u, t), (r, t), (zr, f), (t, t), (s, t), (og, t), (zw, t), (up, f) ])).

tests(most_variables, performance, 
[wt v ru v pt , ~pt v ~qz v ~pu , ~rs v ~r v ~pp ,  ~pq v ~wr v pp , ps v ~w v ~rt , 
~wq v ~qo v wp , ro v qu v ~qw ,  qu v ~ww v rs , po v ~qu v rq , ~qg v z v ~q , ws v ~u v ~s ], 
3200, solution([(pu, f), (pp, f), (wr, f), (wt, t), (ru, t), (pt, t), (qz, t), (rs, t), (pq, t),  (ps, t), (rt, t), 
(wq, t), (qo, t), (wp, t), (ro, t), (qu, t), (qw, t), (ww, t), (po, t), (rq, t), (qg, t), (ws, t), (r, t),  (w, t), 
(z, t), (q, t), (s, t), (u, t) ])).

tests(big_test, performance, 
[~o v ~pz v ~qt v ~rz v ~sz v ~tz v wz v ~gz v or v w v t v ~s v r v ~q v p , 
~o v ~pt v ~qz v ~qz v ~sz v ~tz v wz v ~gz v or v w v t v ~s v r v ~q v ~p , 
~oq v ~pz v ~pz v ~rz v ~st v ~tz v wz v ~gz v or v ~w v t v ~s v r v ~q v p , 
~w v ~or v ~rz , ~q v ~qz , p v ~gz v t , sz v ~or v ~qt , ~qz v ~s v r , 
~o v sz v ~p , pz v ~tz v ~rz , ~o v q v rz , p v qz , w v qz v ~wz ,  
oz v ~w v sz , ~gz v ~pz v ~r , r v gz v rz , 
~o v ~rz v ~pz v ~rz v ~sz v ~tz v wz v ~gz v ~or v w v t v ~s v ~r v q v ~p , 
~oz v ~tt v ~qp v ~rq v ~sz v ~tz v wz v ~gz v ~or v w v t v ~s v ~r v ~q v p ,  
~o v ~pr v ~qz v ~rz v ~wq v ~tz v wz v ~gz v ~or v w v ~t v st v r v q v p , 
o v pz v qz v rz v ~sz v tz v ~wz v ~gz v or v ~w , o v pt v qz v rz v ~sz v tz v ~wz v ~gz v ~or v w , 
o v pz v qz v rz v ~sz v tz v ~wz v ~gz v ~or v ~w , o v pt v qz v ~rz v ~sz v tz v wz v gz v or v w , 
~o v ~ps v ~qz v ~rz v ~sz v ~tz v ~wz v gz v or v w v ~t v s v r v ~q v p , 
~o v ~pz v ~qz v ~rz v ~sz v ~tz v ~wz v ~gz v ~or v w v ~t v ~s v r v q v ~p , 
ot v pz v qz v rz v sz , oq v ps v qz v rz v ~sz , ~o v ~pp v ~qz v rz v ~sz , ~o v ~pt v ~qp v ~rz v sz , 
~o v ~pz v ~qz v ~rz v ~sq v ~tq v ~wz v ~gt v ~or v ~w v ~t v ~s v ~r v ~q v ~p], 
900, solution([( w, f), ( s, f), ( r, f), ( q, f),( pz, t), ( qt, t), ( rz, t), ( sz, t), ( tz, t), 
( wz ,t), ( gz ,t), ( or ,t), ( pt ,t), ( qz ,t), ( oq ,t), ( st ,t), ( oz ,t), ( tt, t), ( qp, t), 
( rq ,t), ( pr ,t), ( wq ,t), ( ps ,t), ( ot ,t), ( pp ,t), ( sq ,t), ( tq ,t), ( gt, t), (t, t), (o, t), (p, t)  ])).

tests(bigger_test, performance, 
[~rt v s v t v ~w v o v tq v ~ts v ~z v ~q v ~p , ~rz v s v t v ~w v o v ~tw v tp v z v q v p , 
r v ~s v t v ~w v ~o v tr v ~tt v z v q v ~p ,  r v ~s v tz v ~w v ~o v tt v ~tz v z v ~q v p ,  
~r v s v t v ~w , ~r v sz v ~t v w ,  ~r v s v ~t v ~w ,  ~rt v ~s v ~t v ~w ,  
~rq v ~s v ~t v ~w v ~o v td v ~tz ,  ~r v ~s v ~t v ~w v ~o v ~td v ts ,  
~r v ~s v ~t v ~w v ~o v ~td v ~t ,  wq v ~o , ~tq v ~r v w , ~w v ~o v ~s , o v rt v ~o , 
~o v o v r , ~r v st , ~r v o v s , tw v t v ~s , oq v s v ~s , s v ~o v ~s , ~p v o , 
t v ~ts v ~r , z v p v ~o , t v tq v ~o , ~t v r v ~p , t v ~w v ~s , s v tz v r ,
 ~r v ~s v ~t v ~w v ~o v ~tw v ~tt v z , 
~r v ~s v ~t v ~w v ~o v ~tq v ~tr v ~z,~sq v t v w v ~o v pt v s v ~qt v ~z v ~qq , 
~s v t v w v ~o v pq v ~s v qw v z v qq , ~s v t v w v ~o v ps v ~s v qz v z v ~qz , 
~s v t v wq v ~o v pw v ~s v qq v ~z v qw , ~sq v t v w v ~o v pq v ~s v qr v ~z v ~qt , 
~s v t v w v ~o v pt v ~s v ~qr v z v qr , ~s v ~t v ~w v ~o v ~pw v s v ~qp v ~z , 
~s v ~t v ~w v ~o v ~pr v ~s v qr v z , ~s v ~t v ~w v ~o v ~pr v ~s v qr v ~z , 
~sq v ~t v ~w v ~o v ~pr v ~s v ~qr v zq , ~s v ~t v ~w v ~o v ~pr v ~s v ~qr v ~z , 
s v t v ~w v ~o v pw v s v ~qt , sq v t v ~w v ~o v pw v ~s v qs , 
s v t v ~w v ~o v pw v ~s v ~qs , sr v t v ~w v ~o v ~pq v s v qq ,
 s v t v ~w v ~o v ~pq v s v ~qs , s v t v ~w v ~o v ~pq v ~sr v qs ,
 s v t v w v ~o v ~pq v sz , s v t v w v ~o v ~pq v ~s , s v t v ~w v o v pq v s ,
 s v t v ~w v oz v pq v ~s , sr v t v ~w v or v ~pw v s , s v tr v ~w v o v ~qp v ~s , s v t v ~w v ~o v pq v s ,
 s v tr v ~w v ~o v pz v ~s , s v tz v ~w v ~o v ~zp v s , st v t v ~w v ~o v ~rp v ~s ,
 s v ~t v w v o v pr v s , s v ~t v w v o v pw v ~s ,sz v ~t v w v o v ~pq v s , s v ~t v w v o v ~pt v ~s ,
 ~s v ~t v w v o v ~ps , ~s v ~t v w v ~o v pp , ~s v ~t v w v ~o v ~pz], 
1200, solution([( pz ,f), (w ,f) ,(p ,f) ,(r ,f),(rt ,t), (tq ,t), (ts ,t),(rz ,t), (tw ,t), (tp ,t),
(tr ,t), (tt ,t), (tz ,t), (sz ,t), (rq ,t), (wq ,t), (st ,t), (oq ,t), (sq ,t), (pt ,t), (qt ,t), 
(qq ,t), (pq ,t), (qw ,t), (ps ,t), (qz ,t), (pw ,t), (qr ,t), (qp ,t), (pr ,t), (zq ,t), (qs ,t), 
(sr ,t), (oz ,t), (or ,t), (zp ,t), (rp ,t), (pp ,t), (s ,t), (t ,t), (o ,t), (z ,t), (q ,t), (td, t) ])).

tests(biggest_test, performance, 
[wq v ~t v sz v ~o v ur v ~z v ~pg , r v w v ~t v s v or , wq v ~t v s v ~o , r v ws v ~t v ~s v or , 
wr v ~t v s v ~o v ~u v z v pg , q v ~r v wg v t v ~s v ~o v u , qr v ~r v w v t v ~s v ~o v ~u , 
q v r v wr v t , q v r v w v ~t , ~w v t , q v ~r v wr v ~t v s v o v ur , 
q v r v w v ~t v s v ~o v ~u v z v ~pg , ~w v ~t v s v o v u v ~zr , ~q v r v ~wg , ~q v ~rg v w , 
~qg v ~r v ~w , q v ~r v ~w v ~t v s v o v ~u v z , 
w v t v ~s v ~o v ~u v z v pg v ~pp , ~q v ~r v w v t v ~s v ~o v ~u v z v ~pg v pp , 
~z v o , ~pp , ~q v ~r v w v t v ~s v o v ut v ~z v pg v pp , 
q v ~r v w v t v ~s v o v ur v ~z v pg v ~pp , ~r v ~t , pp v ~z , pp v t , 
~u v qp v ~q v ~r v ~w v t v ~s v o v ~u v ~z v pg v pp , ~qr v ~r v ~wr v t v ~sr v o v ~ut v ~z v pg v ~pp , 
~qr v ~r v ~wr v t v ~s v o v ~u v ~z v ~pg v pp , ~q v ~r v ~w v tr v ~s v o v ~u v ~z v ~pg v ~pp , 
~qr v ~rr v ~w v t v ~sr v ~or v u v ~z v ~pg v pp , ~qr v ~r v ~w v ~t v s v o v ~u v z v ~pg v pp , 
~q v ~r v ~w v ~t v sr v o v ~u v z v ~pg v ~pp , qs v rs v w v t v rs v o v u v z v pg v pp , 
q v rq v w v t v s v o v u v z v pg v ~pp , qs v r v w v t v s v o v uq v z v ~pg v pp , 
q v r v w v t v s v o v u v z v ~pg v ~pp , qs v rs v w v t v sr v o v u v ~z v ~pg v ~pp , 
qg v r v w v t v s v o v ~uq v z v ~pg v ~pp , q v r v w v t v ~s v ~o v u v z v pg v ~pp , 
qs v rs v w v t v ~s v ~or v u v z v ~pg v pp , ~q v r v w , 
~q v r v ~w , ~q v ~r v w , q v r v w v t v ~s v ~o v u v ~z v ~pg v ~pp , 
qg v r v wr v tw v ~s v ~o v ~ut v z v pg v pp , pg v ~o v o , ~u v ~z v ~t , z v q v ~r , 
pp v pg v p , ~t v r v ~p , ~u v ~r v ~o , r v o v w , ~pg v s v pp , ~s v pp v s , ~t v ~z v w , 
~o v pg , pg v og v zg , ~s v ~o v o , q v p v ~pg , z v ~pp v o , 
pg v qg v rg v w v tg v s v o v us v z v pg v pp , pt v q v r v w v t v s v o v ug v z v pg v ~pp , 
p v qt v r v wg v t v s v o v ug v z v ~pg v pp , p v q v rt v wg v t v sg v o v u v z v ~pg v ~pp , 
p v q v r v w v tt v s v ot v ut v ~z v pg v pp , ~pg v pp v ~s , p v ~z , zt,
pp v q v r v zt v w v tp v s v o v u v z v pg v pp , 
p v qp v zt v r v w v t v st v o v u v z v pg v ~pp , p v q v r v w v zt v tp v s v o v u v z v ~pg v pp , 
qp v r v w v t v ~s v zt v ~o v ~u v z v pg v ~pp , ~o v ~u v zt v ~pg v zt v ~pp , 
q v r v w v t v ~s v zt v ~o v ~u v ~z v ~pg v ~pp , ~r v ~q , r v zt v t , ~o v zt v pg , 
pp v ~r v zt, ~q v r v w v t v zt v ~s ,q v r v wg v tg v ~s v ~o v ~u v zg v ~pg v pp , 
q v r v wg v t v ~s v ~o v ~u v pz v ~pg v ~pp , q v rt v w v t v ~s v ~po v ~pu v ~z v ~pg v ~pp , ~r v ~q , 
r v t , ~zq v p v ~w v ~r v s v pt v ~o v ~u v ~z v qg v ~qq , ~qs v p v ~w v ~r v s v t v ~o v ~u v ~z v ~qg v qq , 
~q v p v ~w v ~r v s v t v ~o v ~u v ~z v ~qg v ~qq , ~q v p v ~w v ~r v sz v ~t v o v u v z v qg v qq , 
~qq v p v ~ws v ~r v s v ~t v o v u v z v qg v ~qq , ~q v p v ~w v ~r v s v ~t v oq v pu v z v ~qg v qq , 
~wq v p v ~w v ~r v s v ~t v o v u v z v ~qg v ~qq , ~q v p v ~w v ~r v s v ~t v o v u v ~pz v qg v qq , 
~q v p v ~w v ~r v s v ~t v o v u v ~z v qg v ~qq , ~pq v p v ~w v ~r v s v ~t v o v pu v ~z v ~qg v qq , 
~q v p v ~w v ~r v s v ~t v o v up v ~z v ~qg v ~qq , ~q v p v ~w v ~r v s v ~tp v o v ~u v z v qg v qq , 
~uq v pp v ~w v ~r v s v ~t v o v ~u v z v qg v ~qq , ~q v p v ~w v ~r v s v ~t v o v ~u v z v ~qg v qq,
~o v pg , pp v ~r , ~q v r v w v t v ~s v o v u v ~z v ~pg v pp , 
w v t v ~s v o v u v ~z v ~pg v ~pp , ~q v ~r v w v t v s v o v ~u v z v pg v pp , ~r v ~q , tp v ~p , w v pg , 
q v sp , ~q v ~r v ~w v t v ~s v ~o v u , ~qt v ~r v ~w v t v ~s v ~o v ~u , 
~q v ~r v ~w v ~t v s v o v u , ~q v ~rt v ~w v ~t v s v ~o v u , ~q v ~r v ~w v ~t v s v ~o v ~u , 
~u v up , q v ~r v ~w v t v ~s , ~w v ~t v s , q v ~r v ~wt v ~t v ~s , 
q v ~r v ~w v ~t v s v o v ~u v ~z , r v w v ~t v s v ~o v ~u v z v ~pg , 
~q v r v w v ~t v s v o v ~u v ~z v pg , ~q v r v w v t v s v ~o v ~u v ~z v ~pg , 
r v w v ~t v ~s v o v u v z v pg , q v r v w v ~t v ~s v o v u v z v ~pg,~o v pg , p v q v s, 
~t v p v ~q, r v t, s v t, q v r, pp v ~r , 
~q v r v w v t v ~s v o v u v ~z v ~pg v pp , w v t v ~s v o v u v ~z v ~pg v ~pp,z v q v ~r , pp v pg v p , 
~t v r v ~p , ~u v ~r v ~o , r v o v w , ~pg v s v pp, ~uw v q v ~p v ~r v t v t v ~s v ~z v ~u v wg v ~ww , 
~wt v q v ~p v ~r v tz v tz v ~s v ~z v ~u v ~wg v ww , ~w v q v ~p v ~qr v rt v t v ~ps v ~z v ~u v ~wg v ~ww , 
~w v qw v ~p v ~r v t v ~t v sz v z v u v wg v ww , ~ww v qw v ~pt v ~r v t v ~t v s v z v wu v wg v ~ww , 
~w v q v ~p v ~r v t v ~tz v sw v z v u v ~wg v ww , ~pw v qw v ~pt v ~r v t v ~tz v s v z v u v ~wg v ~ww , 
~wt v q v ~p v ~r v t v ~tz v s v z v ~u v wg v ww , ~w v q v ~p v ~r v t v ~t v s v z v ~u v wg v ~ww ,
~qw v q v ~p v ~r v t v ~t v s v z v ~u v ~wg v ww , ~w v qw v ~p v ~r v tr v ~t v s v z v ~uq v ~wg v ~ww , 
~w v qw v ~p v ~r v tz v ~tq v s v ~z v u v wg v ww , ~zw v q v ~p v ~r v t v ~t v s v ~z v u v wg v ~ww , 
~wq v q v ~p v ~r v tw v ~t v s v ~z v u v ~wg v ww, ~wq v u v ~z v ~s v t v r v ~p v ~g v ~w v qo v ~qq , 
~qt v u v ~zz v ~s v tz v r v ~p v ~g v ~w v ~qo v qq , 
~q v us v ~zs v ~us v st v r v ~zp v ~g v ~w v ~qo v ~qq , ~q v u v ~z v ~s v t v ~rz v p v g v w v qo v qq ,
~qq v us v ~zt v ~s v t v ~r v p v g v qw v qo v ~qq , ~qs v u v ~z v ~s v t v ~r v pq v g v w v ~qo v qq , 
~zq v uq v ~zt v ~s v t v ~r v p v g v w v ~qo v ~qq , ~qt v u v ~z v ~s v t v ~rz v p v g v ~w v qo v qq , 
~q v u v ~z v ~s v tz v ~r v p v g v ~w v qo v ~qq , ~uq v u v ~z v ~s v t v ~r v p v g v ~w v ~qo v qq , 
~q v us v ~z v ~s v ts v ~rz v p v g v ~wu v ~qo v ~qq , ~q v u v ~z v ~s v t v ~ru v p v ~g v w v qo v qq , 
~gq v us v ~z v ~s v t v ~r v p v ~g v w v qo v ~qq , ~qu v u v ~z v ~s v tq v ~r v p v ~g v w v ~qo v qq], 
2000, count(0)).
