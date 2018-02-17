:- module(kamil_breczko_tests, [tests/3]).

% Zbiór faktów definiujących testy

% dodane po wysłaniu testów
tests(xerror, input("def main(a) = 1+"),no).
tests(xcomment, input("(**)(* (**) (**) (**)*)(**)"),program([])).
tests(xempty, input(""),program([])).
tests(xadder, file('./Tests/xadder.hdml'),yes).

% atom
tests(ok_name, input("def _'main(_'A', _'B') = A"),yes).
tests(illegal_name, input("def else(A, B, C) = A"),no).
tests(incorrect_name, input("def main*program(_) = 1"), no).
tests(illegal_atom, input("def main(_,_)=_"), no).
tests(illegal_var,input("def main(A)= 'A'"),no).
tests(no_space, input("defmain(_)=1"), no).
tests(wrong_par, input("def main(a,b)=[][[[]..[[[]]]]"),no).
tests(wrong_atom, input("def main(A, B)= if 1s0 = A & B then [1] else [0]"),no).

% wzorzec
tests(wrong_pat, file('./Tests/wrong_pat.hdml'), no).
tests(illegal_arg, input("def main(a,b = 2)= 0"),no).
tests(no_arg, input("def main()=1"), no).

% komentarze
tests(wrong_comment, input("(* Test Analizy Leksykalnej *) def main(_) = 1 *)"), no).
tests(empty, input("(* (* Test Analizy (* Leksykalnej *) def main(_) = *)"), program([])).

% definicja
tests(only_def, input("def"),no).
tests(empty_def, input("def main(_) = "),no).
tests(no_assgn, input("def main (a,b) 1"), no).
tests(no_paren, input("def main a,b =1"), no).
tests(no_def, input("if A>1 then 1 else 0"),no).

% wyrazenie if
tests(no_if, input("def main(_)= 1 then 0 else 0 "),no).
tests(no_else, input("def main(_)= if 1 then 0"),no).
tests(no_then, input("def main(_)= if 1 else 0 "),no).
tests(wrong_if, input("def main (a,b)= if  then [a] else 1"),no).
tests(wrong_then, input("def main(_)= if 0 then  else 4"),no).
tests(wrong_else_1, input("def main(a,b)= if 1 then 2 else 3 else 4"),no).
tests(wrong_else_2, input("def main(a,b)= if 1 then 2 else"),no).

% wyrazenie let
tests(no_let, input("def main(_)= _,_= 1,2 in [0]"),no).
tests(no_in, input("def main(_)= let _,_= 1,2"),no).
tests(no_assgn_let, input("def main(_)= let 1 in [0]"),no).
tests(wrong_in_1, input("def main(_)= let A=1 in [0] in [1]"),no).
tests(wrong_in_2, input("def main(_)= let A=1 in"),no).
tests(wrong_assgn, input("def main(a)=let a = in 2*a"),no).

% wyrazenie
tests(no_op, input("def main(_)= 1 2"),no).
tests(wrong_op_bin, input("def op_bin(a,b)= a < 2 > b"),no).
tests(wrong_op_un, input("def op_un(a,b)= a~2"),no).
tests(wrong_op_un_bin, input("def op_un(a,b)= #<4"),no).
	

% bez pozycjonowania
tests(adder, file('./Tests/adder.hdml'), program([
	def('half_adder',
		pair(no,var(no,'A'),var(no,'B')),
		pair(no,op(no,'&',var(no,'A'),var(no,'B')),op(no,'^',var(no,'A'),var(no,'B')))),
 	def('full_adder',
 		pair(no,var(no,'A'),pair(no,var(no,'B'),var(no,'C'))),
 		let(no,pair(no,var(no,'C1'),var(no,'S1')),call(no,'half_adder',pair(no,var(no,'A'),var(no,'B'))),
 		 let(no,pair(no,var(no,'C2'),var(no,'S2')),call(no,'half_adder',pair(no,var(no,'S1'),var(no,'C'))),
 		 pair(no,op(no,'|',var(no,'C1'),var(no,'C2')),var(no,'S2'))))),
 	def('ripple_carry_adder',
 		pair(no,var(no,'A'),pair(no,var(no,'B'),var(no,'C'))),
 		let(no,var(no,'N'),op(no,'#',var(no,'A')),if( no,op(no,'=',var(no,'N'),num(no,0)),
 		 pair(no,var(no,'C'),empty(no)),let(no,pair(no,var(no,'C1'),var(no,'S1')),
 		 call(no,'full_adder',pair(no,bitsel(no,var(no,'A'),num(no,0)),pair(no,bitsel(no,var(no,'B'),num(no,0)),
 		 var(no,'C')))),let(no,pair(no,var(no,'C2'),var(no,'S2')),call(no,'ripple_carry_adder',
 		 pair(no,bitsel(no,var(no,'A'),op(no,'-',var(no,'N'),num(no,1)),num(no,1)),pair(no,
 		 bitsel(no,var(no,'B'),op(no,'-',var(no,'N'),num(no,1)),num(no,1)),var(no,'C1')))),
 		 pair(no,var(no,'C2'),op(no,'@',var(no,'S2'),var(no,'S1')))))))),
 	def('log_depth_adder_aux',
 		pair(no,var(no,'A'),pair(no,var(no,'B'),var(no,'C'))),
 		let(no,var(no,'N'),op(no,'#',var(no,'A')),if(no,op(no,'=',var(no,'N'),num(no,1)),
 		 let(no,var(no,'P'),op(no,'^',var(no,'A'),var(no,'B')),pair(no,
 		 var(no,'P'),pair(no,op(no,'&',var(no,'A'),var(no,'B')),op(no,'^',var(no,'P'),var(no,'C'))))),
 		 let(no,var(no,'H'),op(no,'/',var(no,'N'),num(no,2)),let(no,pair(no,var(no,'P1'),pair(no,
 		 var(no,'G1'),var(no,'S1'))),call(no,'log_depth_adder_aux',pair(no,
 		 bitsel(no,var(no,'A'),op(no,'-',var(no,'H'),num(no,1)),num(no,0)),pair(no,
 		 bitsel(no,var(no,'B'),op(no,'-',var(no,'H'),num(no,1)),num(no,0)),
 		 var(no,'C')))),let(no,pair(no,var(no,'P2'),pair(no,var(no,'G2'),var(no,'S2'))),
 		 call(no,'log_depth_adder_aux',pair(no,bitsel(no,var(no,'A'),op(no,'-',var(no,'N'),num(no,1)),var(no,'H')),
 		 pair(no,bitsel(no,var(no,'B'),op(no,'-',var(no,'N'),num(no,1)),var(no,'H')),
 		 op(no,'|',var(no,'G1'),op(no,'&',var(no,'C'),var(no,'P1')))))),pair(no,op(no,'&',var(no,'P1'),var(no,'P2')),
 	     pair(no,op(no,'|',var(no,'G2'),op(no,'&',var(no,'G1'),var(no,'P2'))),
 		 op(no,'@',var(no,'S2'),var(no,'S1')))))))))),
 	def('log_depth_adder',
 		pair(no,var(no,'A'),pair(no,var(no,'B'),var(no,'C'))),
 		if(no,op(no,'=',op(no,'#',var(no,'A')),num(no,0)),pair(no,var(no,'C'),empty(no)),
 		 let(no,pair(no,var(no,'P'),pair(no,var(no,'G'),var(no,'S'))),call(no,'log_depth_adder_aux',
 		 pair(no,var(no,'A'),pair(no,var(no,'B'),var(no,'C')))),pair(no,op(no,'|',var(no,'G'),
 		 op(no,'&',var(no,'P'),var(no,'C'))),var(no,'S'))))),
 	def('main',
 		pair(no,var(no,'A'),pair(no,var(no,'B'),var(no,'C'))),
 		let(no,pair(no,var(no,'C1'),var(no,'S1')),call(no,'ripple_carry_adder',pair(no,var(no,'A'),
 		 pair(no,var(no,'B'),var(no,'C')))),let(no,pair(no,var(no,'C2'),var(no,'S2')),
 		 call(no,'log_depth_adder',pair(no,var(no,'A'),pair(no,var(no,'B'),var(no,'C')))),op(no,
 		 '~',op(no,'|',op(no,'^',var(no,'C1'),var(no,'C2')),call(no,'or_of',op(no,'^',var(no,'S1'),
 		 var(no,'S2'))))))))
])).

tests(choice_bits, file('./Tests/choice_bit.hdml'), program([
		def('choice_bit',
			var(no,'A'),
			let(no,var(no,'N'),op(no,'*',var(no,'A'),op(no,'#',var(no,'A'))),
		 	 if(no,op(no,'=',op(no,'/',var(no,'N'),var(no,'A')),num(no,1)),
			 bitsel(no,var(no,'A'),num(no,1)),bitsel(no,var(no,'A'),empty(no))))),
 		def('choice_bit_exp',
 			var(no,'A'),
 			bitsel(no,op(no,'+',var(no,'A'),num(no,42)),let(no,var(no,'N'),op(no,'*',var(no,'A'),num(no,2)),
 		 	 if(no,op(no,'=',op(no,'/',var(no,'N'),num(no,2)),num(no,1)),bitsel(no,var(no,'A'),num(no,1)),
 			 bitsel(no,var(no,'A'),empty(no)))))),
 		def('choice_bits',
 			pair(no,var(no,'A'),var(no,'B')),
 			let(no,var(no,'M'),op(no,'-',var(no,'A'),num(no,1)),if(no,op(no,'=',op(no,'%',var(no,'N'),
 			 num(no,2)),num(no,1)),bitsel(no,var(no,'A'),op(no,'%',var(no,'N'),num(no,2)),
 			 op(no,'@',num(no,1),var(no,'N'))),bitsel(no,var(no,'A'),call(no,'choice_bit',var(no,'_a')),
 			 bit(no,num(no,0)))))),
 	def(
 		'choice_bits_exp',
 		pair(no,var(no,'A'),var(no,'B')),
 		bitsel(no,let(no,var(no,'N'),num(no,1),if(no,op(no,'=',var(no,'A'),var(no,'N')),var(no,'A'),var(no,'B'))),
 		 let(no,var(no,'N'),num(no,42),if(no,op(no,=,op(no,'^',var(no,'B'),num(no,2)),var(no,'N')),
 		 bitsel(no,var(no,'A'),num(no,1)),bitsel(no,var(no,'A'),let(no,var(no,'N'),num(no,1),
 		 if(no,op(no,'=',var(no,'N'),var(no,'_a')),var(no,'_a'),bit(no,num(no,42))))))),
 		 if(no,op(no,'=',var(no,'A'),var(no,'B')),var(no,'A'),var(no,'B')))),
 	def('bit',pair(no,var(no,'A'),var(no,'N')),bitsel(no,var(no,'A'),var(no,'N'))),
 	def('bits',pair(no,var(no,'A'),pair(no,var(no,'F'),var(no,'T'))),bitsel(no,var(no,'A'),var(no,'F'),var(no,'T'))),
 	def('bits',
 		var(no,'a'),
 		bitsel(no,bitsel(no,bitsel(no,bitsel(no,var(no,'a'),num(no,4)),num(no,3)),num(no,2)),
 		 num(no,1),num(no,0))),
 	def('bit',var(no,'a'),bitsel(no,bitsel(no,var(no,'a'),num(no,4),num(no,2)),num(no,1))),
 	def('main',
 		pair(no,var(no,'A'),var(no,'B')),
 		let(no,var(no,'H1'),call(no,'choice_bit',var(no,'A')),let(no,var(no,'H2'),
 		 call(no,'choice_bit_exp',var(no,'A')),let(no,var(no,'H3'),
 		 call(no,'choice_bits',pair(no,var(no,'A'),var(no,'B'))),
 		 let(no,var(no,'H4'),call(no,'choice_bits_exp',pair(no,var(no,'A'),var(no,'B'))),
 		 if(no,op(no,'=',var(no,'H1'),num(no,0)),if(no,op(no,'=',var(no,'H2'),num(no,0)),
 		 if(no,op(no,'=',var(no,'H3'),num(no,0)),if(no,op(no,'=',var(no,'H4'),num(no,0)),
 		 bitsel(no,bitsel(no,var(no,'B'),num(no,42),num(no,1)),op(no,'/',num(no,42),num(no,2)),
 		 var(no,'_a')),bitsel(no,bitsel(no,var(no,'B'),empty(no)),empty(no))),empty(no)),
 		 bitsel(no,var(no,'_a'),bit(no,num(no,1)))),bitsel(no,var(no,'A'),bitsel(no,var(no,'_a'),
 		 bitsel(no,var(no,'_a'),bitsel(no,var(no,'_a'),bitsel(no,empty(no),empty(no),empty(no))))))))))))
])).


tests(comment, file('./Tests/comment.hdml'), program([
		def('_let',pair(no,var(no,'_A'),pair(no,var(no,'B1'),pair(no,var(no,'_C'),var(no,'D')))),num(no,1))
])).

tests(no_parenthesis, file('./Tests/no_parenthesis.hdml'), program([
 		def('program',
 			pair(no,var(no,'A'),pair(no,var(no,'B'),var(no,'C'))),
 			if(no,op(no,'=',var(no,'A'),num(no,0)),op(no,'<>',op(no,'@',
 		     op(no,'+',op(no,'*',var(no,'A'),num(no,1)),num(no,2)),
 			 op(no,'|',op(no,'&',op(no,'-',num(no,3)),op(no,'#',num(no,42))),num(no,42))),
 		     op(no,'^',op(no,'-',op(no,'%',op(no,'/',num(no,222),num(no,2)),var(no,'B')),var(no,'C')),
 			 op(no,'~',var(no,'A')))),let(no,var(no,'N'),op(no,'-',var(no,'B')),pair(no,
 			 op(no,'@',var(no,'A'),num(no,3)),pair(no,op(no,'*',var(no,'B'),num(no,2)),
 			 pair(no,op(no,'-',var(no,'N'),num(no,1)),op(no,'<',var(no,'A'),num(no,3)))))))),
 		def('ok',
 			var(no,'A'),
 			if(no,
 			 if(no,if(no,op(no,'>',var(no,'A'),num(no,1)),var(no,'A'),num(no,0)),num(no,1),num(no,0)),
 			 num(no,1),num(no,0)))
])).

tests(operators, file('./Tests/op_bin_un.hdml'), program([
		def('OR',
			pair(no,var(no,'A'),var(no,'B')),
			if(no,op(no,'<>',num(no,0),op(no,'|',var(no,'A'),var(no,'B'))),bit(no,num(no,1)),bit(no,num(no,0)))),
 		def('AND',
 			pair(no,var(no,'A'),var(no,'B')),
 			let(no,var(no,'res'),op(no,'&',var(no,'A'),var(no,'B')),bit(no,var(no,'res')))),
 		def('XOR',
 			pair(no,var(no,'A'),var(no,'B')),
 			if(no,op(no,'&',op(no,'>',op(no,'|',var(no,'A'),var(no,'B')),num(no,0)),
 			 op(no,'<',op(no,'&',var(no,'A'),var(no,'B')),num(no,1))),
 			 bit(no,op(no,'^',var(no,'A'),var(no,'B'))),bit(no,num(no,0)))),
		def('NOT',
			var(no,'A'),
			if(no,op(no,'=',var(no,'A'),num(no,0)),bit(no,num(no,0)),if(no,
			 op(no,'>=',var(no,'A'),num(no,1)),bit(no,op(no,'-',op(no,'-',var(no,'A')))),
			 bit(no,op(no,'~',op(no,'~',op(no,'~',var(no,'A')))))))),
	 	def('RIGHT',
	 		var(no,'A'),
	 		let(no,var(no,'N'),op(no,'#',var(no,'A')),if(no,op(no,'<=',var(no,'N'),num(no,2)),
	 		 bitsel(no,var(no,'A'),num(no,2)),bitsel(no,var(no,'A'),var(no,'N'),num(no,2))))),
	 	def('Left',
	 		var(no,'A'),
	 		let(no,var(no,'N'),op(no,'#',op(no,'#',var(no,'A'))),if(no,op(no,'>',var(no,'N'),
	 		 num(no,1)),bitsel(no,var(no,'A'),op(no,'-',var(no,'N'),num(no,1)),num(no,1)),
	 		 bitsel(no,var(no,'A'),num(no,1))))),
	 	def('test',
	 		var(no,'a'),
	 		op(no,'*',op(no,'~',op(no,'#',op(no,'-',op(no,'#',var(no,'a'))))),
	 		 op(no,'#',op(no,'~',op(no,'-',var(no,'a'))))))])).

tests(parenthesis, file('./Tests/parenthesis.hdml'), program([
 	def('program',
 		pair(no,var(no,'A'),wildcard(no)),
 		let(no,pair(no,pair(no,var(no,'B'),var(no,'C')),wildcard(no)),pair(no,op(no,'#',pair(
 		 no,op(no,'*',var(no,'A'),op(no,'+',var(no,'A'),num(no,1))),op(no,'-',var(no,'A'),
 		 op(no,'*',var(no,'A'),num(no,2))))),empty(no)),
 		 if(no,op(no,'=',var(no,'A'),num(no,1)),op(no,'~',op(no,'&',var(no,'B'),var(no,'C'))),
 		 if(no,op(no,'|',op(no,'&',op(no,'>',var(no,'A'),num(no,1)),op(no,'<',var(no,'B'),
 		 op(no,'-',num(no,1)))),op(no,'&',op(no,'<',var(no,'A'),op(no,'-',num(no,1))),
 		 op(no,'>',var(no,'B'),num(no,1)))),bitsel(no,var(no,'A'),num(no,1),var(no,'B')),
 		 op(no,'&',op(no,'-',op(no,'|',var(no,'B'),var(no,'C'))),var(no,'A'))))))])).


tests(premature_ending, file('./Tests/premature_ending.hdml'), program([
	def('loop_1',
		pair(no,var(no,'a'),var(no,'b')),
		if(no,op(no,'=',var(no,'a'),var(no,'_a')),call(no,'_a',var(no,'a')),call(no,'_a',var(no,'b')))),
 	def('loop_2',
 		pair(no,var(no,'a'),var(no,'b')),
 		let(no,wildcard(no),call(no,'_a',var(no,'a')),call(no,'_a',var(no,'b')))),
 	def('function',
 		var(no,'a'),
 		call(no,'loop',pair(no,let(no,var(no,'N'),op(no,'*',var(no,'a'),var(no,'c')),var(no,'N')),
 		 if(no,var(no,'a'),var(no,'b'),call(no,'loop',pair(no,let(no,var(no,'N'),op(no,'*',
 		 var(no,'a'),var(no,'b')),var(no,'N')),
 		 if(no,var(no,'a'),var(no,'b'),call(no,'loop',pair(no,var(no,'_a'),var(no,'_a')))))))))),
 	def('choice_bit',pair(no,wildcard(no),wildcard(no)),bitsel(no,var(no,'_a'),var(no,'_a'))),
 	def('choice_bits',pair(no,wildcard(no),wildcard(no)),bitsel(no,var(no,'_a'),var(no,'_a'),var(no,'_a'))),
 	def('call_function',pair(no,wildcard(no),wildcard(no)),call(no,'_a',var(no,'_a'))),
 	def('op_bin0',pair(no,var(no,'a'),var(no,'b')),op(no,'+',var(no,'a'),var(no,'b'))),
 	def('op_bin1',pair(no,var(no,'a'),var(no,'b')),op(no,'&',var(no,'b'),var(no,'a'))),
 	def('op_bin2',pair(no,var(no,'a'),var(no,'b')),op(no,'|',var(no,'b'),var(no,'a'))),
 	def('op_bin3',pair(no,var(no,'a'),var(no,'b')),op(no,'<',var(no,'a'),num(no,2))),
	def('op_bin4',wildcard(no),pair(no,var(no,'a'),var(no,'b')))
])).

tests(undefined, file('./Tests/undefined.hdml'), program([
	def('_a',
		pair(no,wildcard(no),wildcard(no)),
		if(no,op(no,'-',op(no,'+',op(no,'*',var(no,'_a'),var(no,'_a')),op(no,'/',var(no,'_a'),
		 var(no,'_a'))),var(no,'_a')),var(no,'_a'),let(no,wildcard(no),op(no,'+',var(no,'_a'),
		 num(no,42)),let(no,wildcard(no),var(no,'_a'),bit(no,call(no,'_a',if(no,var(no,'_a'),
		 var(no,'_a'),bitsel(no,var(no,'_a'),var(no,'_a'),bit(no,var(no,'_a')))))))))),
 	def('_a',wildcard(no),var(no,'_a'))
])).

% z pozycjonowaniem
tests(presentation, input("def half_adder(A, B) = A & B, A ^ B"), program([
	def('half_adder',
		pair(file('./Tests/test',1,16,15,4),var(file('./Tests/test',1,16,15,1),'A'),var(file('./Tests/test',1,19,18,1),'B')),
		pair(file('./Tests/test',1,24,23,12),
		 op(file('./Tests/test',1,24,23,5),'&',var(file('./Tests/test',1,24,23,1),'A'),var(file('./Tests/test',1,28,27,1),'B')),
		 op(file('./Tests/test',1,31,30,5),'^',var(file('./Tests/test',1,31,30,1),'A'),var(file('./Tests/test',1,35,34,1),'B'))))
])).
tests(windows, file('./Tests/windows.hdml'), program([
	def(
		'main',
		var(file('./Tests/windows.hdml',3,12,52,1),'X'),
		let(file('./Tests/windows.hdml',4,2,60,74),var(file('./Tests/windows.hdml',4,6,64,1),'N'),
		 num(file('./Tests/windows.hdml',4,10,68,1),1),if(file('./Tests/windows.hdml',5,2,75,59),
		 op(file('./Tests/windows.hdml',5,5,78,5),'<',var(file('./Tests/windows.hdml',5,5,78,1),'N'),
		 var(file('./Tests/windows.hdml',5,9,82,1),'X')),bit(file('./Tests/windows.hdml',5,16,89,5),
		 var(file('./Tests/windows.hdml',5,18,91,1),'X')),if(file('./Tests/windows.hdml',7,3,105,29),
		 var(file('./Tests/windows.hdml',7,7,109,1),'X'),var(file('./Tests/windows.hdml',7,15,117,1),'X'),
		 bitsel(file('./Tests/windows.hdml',8,8,127,7),var(file('./Tests/windows.hdml',8,8,127,1),'X'),
		 var(file('./Tests/windows.hdml',8,10,129,1),'N'),var(file('./Tests/windows.hdml',8,13,132,1),'X'))))))
])).
tests(linux, file('./Tests/mac.hdml'), program([
	def('main',
		var(file('./Tests/mac.hdml',3,12,46,1),'X'),
		let(file('./Tests/mac.hdml',4,2,53,70),var(file('./Tests/mac.hdml',4,6,57,1),'N'),
		 num(file('./Tests/mac.hdml',4,10,61,1),1),if(file('./Tests/mac.hdml',5,2,67,56), 
		 op(file('./Tests/mac.hdml',5,5,70,5),'<',var(file('./Tests/mac.hdml',5,5,70,1),'N'),var(file('./Tests/mac.hdml',5,9,74,1),'X')),
		 bit(file('./Tests/mac.hdml',5,16,81,5),var(file('./Tests/mac.hdml',5,18,83,1),'X')),if(file('./Tests/mac.hdml',7,3,95,28),
		 var(file('./Tests/mac.hdml',7,7,99,1),'X'),var(file('./Tests/mac.hdml',7,15,107,1),'X'),
		 bitsel(file('./Tests/mac.hdml',8,8,116,7),var(file('./Tests/mac.hdml',8,8,116,1),'X'),
		 var(file('./Tests/mac.hdml',8,10,118,1),'N'),var(file('./Tests/mac.hdml',8,13,121,1),'X'))))))
])).

tests(mac, file('./Tests/linux.hdml'), program([
	def('main',
		var(file('./Tests/linux.hdml',2,12,46,1),'X'),
		let(file('./Tests/linux.hdml',3,2,53,70),var(file('./Tests/linux.hdml',3,6,57,1),'N'),
		 num(file('./Tests/linux.hdml',3,10,61,1),1),if(file('./Tests/linux.hdml',4,2,67,56),
		 op(file('./Tests/linux.hdml',4,5,70,5),'<',var(file('./Tests/linux.hdml',4,5,70,1),'N'),
		 var(file('./Tests/linux.hdml',4,9,74,1),'X')),bit(file('./Tests/linux.hdml',4,16,81,5),
		 var(file('./Tests/linux.hdml',4,18,83,1),'X')),if(file('./Tests/linux.hdml',6,3,95,28),
		 var(file('./Tests/linux.hdml',6,7,99,1),'X'),var(file('./Tests/linux.hdml',6,15,107,1),'X'),
		 bitsel(file('./Tests/linux.hdml',7,8,116,7),var(file('./Tests/linux.hdml',7,8,116,1),'X'),
		 var(file('./Tests/linux.hdml',7,10,118,1),'N'),var(file('./Tests/linux.hdml',7,13,121,1),'X'))))))
])).

