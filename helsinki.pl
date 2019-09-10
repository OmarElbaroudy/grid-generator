

%
grid_build(N,M):-
		N >= 0,
		length(M,N),
		grid_build_helper(M,N).

grid_build_helper([],_).
grid_build_helper([H|T],N):-
		length(H,N),
		grid_build_helper(T,N).
		
%
grid_gen(N,M):-
		grid_build(N,M),
		grid_gen_helper(M,N).
		
grid_gen_helper([],_).
grid_gen_helper([H|T],N):-
		grid_gen_helper2(H,N),
		grid_gen_helper(T,N).
		
grid_gen_helper2([],_).
grid_gen_helper2([H|T],N):-
		num_gen(1,N,R),
		member(H,R),
		grid_gen_helper2(T,N).
		
%
num_gen(_,0,[]).	
num_gen(X,X,[X]).
num_gen(F,L,R):-
		F < L,
		R = [F|T],
		F1 is F+1,
		num_gen(F1,L,T).
	
	
%
check_num_grid([]).
check_num_grid(G):-
		get_max(G,M),
		num_gen(1,M,R),
		check_num_grid_helper(G,R).
		
check_num_grid_helper([],_).		
check_num_grid_helper([H|T],R):-
		check_num_grid_helper2(H,R,R1),
		check_num_grid_helper(T,R1).
		
check_num_grid_helper2([],R,R).
check_num_grid_helper2([H|T],R,R2):-
		delete(R,H,R1),
		check_num_grid_helper2(T,R1,R2).


get_max([],0).		
get_max(G,M):-
		get_max_helper(G,0,M).

get_max_helper([],M,M).
get_max_helper([H|T],MAXSOFAR,M):-
		get_max_helper2(H,MAXSOFAR,M1),
		get_max_helper(T,M1,M).

get_max_helper2([],M,M).
get_max_helper2([H|T],MAXSOFAR,M):-
		maxOf(H,MAXSOFAR,M1),
		get_max_helper2(T,M1,M).


maxOf(A,B,A):-
		A >= B.
maxOf(A,B,B):-
		B > A.
		
		
%
acceptable_distribution(G):-
		trans(G,G1),
		acceptable_distribution_helper(G,G1).

acceptable_distribution_helper([],[]).	
acceptable_distribution_helper([H1|T1],[H2|T2]):-
		\+identical(H1,H2),
		acceptable_distribution_helper(T1,T2).
		
%
row_col_match([]).
row_col_match(G):-
		trans(G,G1),
		row_col_match(G,G1).
row_col_match([],_).
row_col_match([H|T],Trans):-
		member(H,Trans),
		row_col_match(T,Trans).


%
acceptable_permutation([],[]).
acceptable_permutation(L,R):-
		permutation(L,R),
		no_common(L,R).
		
no_common([],[]).
no_common([H1|T1],[H2|T2]):-
		H1 \= H2,
		no_common(T1,T2).


%
trans([[]|_],[]).
trans(M,[FR|R]):-
		trans_helper(M,FR,M1),
		trans(M1,R).
		
trans_helper([],[],[]).
trans_helper([[H|T]|R],[H|H1],[T|T1]):-
		trans_helper(R,H1,T1).

		
%
distinct_rows([]).
distinct_rows([H|T]):-
		distinct_rows_helper(H,T),
		distinct_rows(T).

distinct_rows_helper(_,[]).
distinct_rows_helper(L,[H|T]):-
		\+identical(L,H),
		distinct_rows_helper(L,T).


identical([],[]).		
identical([H1|T1],[H2|T2]):-
		H1 == H2,
		identical(T1,T2).
		
		
%
distinct_columns(M):-
		trans(M,M1),
		distinct_rows(M1).


%
helsinki(N,G):-
		grid_gen(N,G),
		check_num_grid(G),
		acceptable_distribution(G),
		row_col_match(G),
		distinct_rows(G),
		distinct_columns(G).