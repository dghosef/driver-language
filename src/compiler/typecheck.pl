:- set_prolog_flag(double_quotes, chars).
use_module(tokenize).
use_module(parse).
check_driver(Stmts, Params, A1, C) :-
  M = memmap{},
  V = varmap{},
  S = syncstatusmap{},
  check_params(Params, params{}, P),
  A = assumptionsmap{},
  check_stmts(M, V, S, P, A, Stmts, _, _, _, A1, C).

check_params([], P, P).
check_params([Param|Rest], P, P2) :-
  check_param(Param, P, P1),
  check_params(Rest, P1, P2).
check_param([Name, Size], P, P.put(Name, Size)) :-
  Size =< 32,
  \+ P.get(Name).

% Block rule
check_stmts(M, V, S, P, A, [Stmt|Rest], M2, V2, S2, A2, [C|C1]) :-
  check_stmt(M, V, S, P, A, Stmt, M1, V1, S1, A1, C),
  !, % Once weve found a statement, don't go back
  check_stmts(M1, V1, S1, P, A1, Rest, M2, V2, S2, A2, C1).

% EmptyBlock rule
check_stmts(M, V, S, _, A, [], M, V, S, A, []) :- check_S(S).
% Assume rule
% check_stmt(mem{}, var{}, sync{}, param{}, assump{}, [assume, 123, _, _, 5, 20, 5], M1, V1, S1, A1).
check_stmt(M, V, S, _, A, Stmt, M1, V, S1, A1, C) :-
  Stmt = [assume, Addr, _, _, Start, End, Val],
  Len is End - Start + 1,
  paddeddec2bin(Val, ValArr, Len),
  memGet(M, Addr, OGVal),
  write_bitrange(Addr, Start, End, ValArr, OGVal, M, M1),
  S1 = S.put(Addr, yes),
  \+A.get(Addr),
  A1 = A.put(Addr, (Start, End, Val)),
  C = []. % Assumes are @ beginning of function so no codegen here

% VarDecl-Num rule
% Stmt = [assume, Addr, Varname, Paramname, Start, End, Val],
% check_stmt(mem{}, var{}, sync{}, param{}, assump{}, [vd_num, _, name, _, _, _, 2863311530], M1, V1, S1, A1).
check_stmt(M, V, S, P, A, Stmt, M, V1, S, A, C) :-
  Stmt = [vd_num, _, Varname, _, _, _, Val],
  \+ P.get(Varname),
  dec2bigbin(Val, ValArr),
  V1 = V.put(Varname, ValArr), 
  C = [long, Varname, '=', Val, ';'].

% VarDecl-Addr rule
% Stmt = [assume, Addr, Varname, Paramname, Start, End, Val],
% check_stmt(mem{}, var{}, sync{}, param{}, assump{}, [vd_num, _, name, _, _, _, 2863311530], M1, V1, S1, A1).
check_stmt(M, V, S, P, A, Stmt, M, V1, S1, A, C) :-
  Stmt = [vd_addr, Addr, Varname, _, _, _, _],
  sync_status(read, Addr, S, S1),
  S1.get(Addr, yes) == yes,
  \+ P.get(Varname),
  read(M, Addr, Val),
  V1 = V.put(Varname, Val),
  atom_number(Addr, AddrNum),
  read_inst(AddrNum, Varname, C).

% VarUpdate Num rule
% check_stmt(mem{}, var{name:[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]}, sync{}, param{}, assump{}, [vu_num, _, name, _, 3, 10, 5], M1, V2, S1, A1).
check_stmt(M, V, S, P, A, Stmt, M, V1, S, A, C) :-
  Stmt = [vu_num, _, Varname, _, Start, End, Val],
  \+ P.get(Varname),
  Len is End - Start + 1,
  paddeddec2bin(Val, ValArr, Len),
  OGVal = V.get(Varname),
  write_bitrange(Varname, Start, End, ValArr, OGVal, V, V1),
  C = [update_var, '(', Varname, ',', Start, ',', End, ',', Val,')', ';'].

% VarUpdate param rule
% check_stmt(mem{}, var{name:[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]}, sync{}, param{p:3}, assump{}, [vu_param, _, name, p, 3, 10, _], M1, V2, S1, A1).
check_stmt(M, V, S, P, A, Stmt, M, V1, S, A, C) :-
  Stmt = [vu_param, _, Varname, Paramname, Start, End, _],
  ParamSize = P.get(Paramname),
  OGVal = V.get(Varname),
  duplicate_list(u, ParamSize, Unknowns),
  write_bitrange(Varname, Start, End, Unknowns, OGVal, V, V1),
  C = [update_var, '(', Varname, ',', Start, ',', End, ',', Paramname,')', ';'].

% write rule
/*
check_stmt(mem{123:[1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]},v{name:[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]},sync{},param{},assump{},[write, 1054, name, _, _, _, _],M1, V1, S1, A1).
should fail because write dependency not met:
check_stmt(mem{123:[1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]},v{name:[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]},sync{},param{},assump{},[write, 1054, name, _, _, _, _],M1, V1, S1, A1).
*/
check_stmt(M, V, S, _, A, Stmt, M1, V, S2, A, C) :-
  Stmt = [write, Addr, Varname, _, _, _, _],
  sync_status(write, Addr, S, S1),
  can_write(Addr, M, S1),
  M1 = M.put(Addr, V.get(Varname)),
  S2 = S1.put(Addr, no),
  atom_number(Addr, AddrNum),
  write_inst(AddrNum, Varname, C).
check_S_pairs([_-Status|T]) :-
  Status == yes,
  check_S_pairs(T).
check_S_pairs([Addr-_|T]) :-
  atom_number(Addr, N),
  \+must_sync(N),
  check_S_pairs(T).
check_S_pairs([]).
check_S(S) :-
  dict_pairs(S, _, Pairs),
  check_S_pairs(Pairs).
  

paddeddec2bin(Val, Res, Len) :-
  dec2bin(Val, Res1),
  length(Res1, L),
  Len1 is Len - L,
  duplicate_list(0, Len1, Res2),
  append(Res2, Res1, Res).
  

dec2bin(N,L) :- dec2bin(N,[],L).

dec2bin(0,L,[0|L]).
dec2bin(1,L,[1|L]).
dec2bin(N,L,R):- 
    N > 1,
    X is N mod 2,
    Y is N // 2,  
    dec2bin(Y,[X|L],R).

binpad(Bin, Res) :-
  length(Bin, Len),
  Len < 32,
  binpad([0|Bin], Res).

binpad(Bin, Bin) :- 
  length(Bin, Len),
   Len == 32.

dec2bigbin(In, Res) :-
  dec2bin(In, Bin),
  binpad(Bin, Res).

valid_address(A) :- 
  atom_number(A, N),
  N >= 0.
% https://stackoverflow.com/questions/8519203/prolog-replace-an-element-in-a-list-at-a-specified-index
replace(I, L, E, K) :-
  nth0(I, L, _, R),
  nth0(I, K, E, R).

update_bit_array(OGVal, [Val|Rest], Pos, Updated) :-
  Ps is Pos,
  replace(Ps, OGVal, Val, X),
  NewPos is Ps + 1,
  update_bit_array(X, Rest, NewPos, Updated).

update_bit_array(OGVal, [], _, OGVal).

valid_bitrange(Start, End) :-
  0 =< Start,
  Start =< End,
  End =< 31.

write_bitrange(Addr, Start, End, Val, OGVal, M, M.put(Addr, NewVal)) :-
  valid_bitrange(Start, End),
  length(Val, ValSize),
  ValSize is (End - Start + 1),
  update_bit_array(OGVal, Val, Start, NewVal).
  
memGet(M, Addr, Out) :-
  valid_address(Addr),
  Out = M.get(Addr, [u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u, u]).


can_write(Addr, M, S1) :-
  atom_number(Addr, N),
  write_dependencies(N, Dependencies),
  write_dependencies_satisfied(Dependencies, M, S1).

write_dependencies_satisfied([H|T], M, S) :-
  write_dependency_satisfied(H, M, S),
  write_dependencies_satisfied(T, M, S).

write_dependencies_satisfied([], _, _).

write_dependency_satisfied([Addr, Start, End, [H|T]], M, S) :-
  Start =< End,
  atom_number(Addr5, Addr),
  S.get(Addr5, yes) == yes,
  duplicate_list(u, 32, Default),
  atom_number(AddrAtom, Addr),
  length(T, Len),
  Ps is 31 - Start - Len,
  nth0(Ps, M.get(AddrAtom, Default), Val),
  Val == H,
  write_dependency_satisfied([Addr, Start, End, T], M, S).
write_dependency_satisfied([_, _, _, []], _, _).
  
read(M, Addr, Val) :- 
  memGet(M, Addr, Val1),
  atom_number(Addr, AddrVal),
  readProperty(AddrVal, Val2),
  combine(Val1, Val2, Val).

combine([H1|T], [H2|T1], [H1|T2]) :-
  H2 == l,
  combine(T, T1, T2).

combine([_|T], [H2|T1], [H2|T2]) :-
  \+ H2 == l,
  combine(T, T1, T2).

combine([], [], []).

duplicate_list(Elem, N, [Elem|Rest]) :-
  N > 0,
  N1 is N - 1,
  duplicate_list(Elem, N1, Rest).

duplicate_list(_, 0, []).

range(Start, End, [Start|Rest]) :-
  Start =< End,
  Start1 is Start + 1,
  range(Start1, End, Rest).
range(Start, End, []):-
  Start > End.

sync_status(_, A, _, syncstatusmap{}) :-
  atom_number(A, 15).

sync_status(Action, AddrAtom, S, S2) :-
  atom_number(AddrAtom, Addr),
  sync_rules(Action, Addr, Synced),
  update_sync(Synced, S, S2).

sync_status(Action, AddrAtom, S, S) :-
  atom_number(AddrAtom, Addr),
  \+ sync_rules(Action, Addr, _).

update_sync([], S, S).

update_sync([HVal|T], S, S2) :-
  atom_number(H, HVal),
  S.get(H) == no,
  S1 = S.put(H, yes),
  update_sync(T, S1, S2).

update_sync([HVal|T], S, S2) :-
  atom_number(H, HVal),
  \+ S.get(H) == no,
  update_sync(T, S, S2).


output_code(Name, Params, Instrs, A) :- 
  write('#include "compD.h"\n'),
  write(void),
  write(' '),
  write(Name),
  write('('),
  output_params(Params),
  write(')'),
  write(' '),
  write('{'),
  nl,
  dict_pairs(A, _, Pairs),
  output_assumptions(Pairs),
  output_instrs(Instrs),
  write('}').
output_assumptions([H|T]) :-
  output_assumption(H),
  nl,
  output_assumptions(T).
output_assumptions([]).
output_assumption((A-(B, C, D))) :-
  tab(4),
  write(check_assump),
  Start is 31 - C,
  End is 31 - B,
  write('('),
  write(A),
  write(','),
  write(Start),
  write(','),
  write(End),
  write(','),
  write(D),
  write(')'),
  write(';').
output_instrs([H|T]) :-
  tab(4),
  output_instr(H),
  output_instrs(T).
output_instrs([]).

output_instr([H|T]) :-
  write(H),
  write(' '),
  output_instr(T).
output_instr([]) :- nl.

write_param(Name) :- write(long), write(' '), write(Name).
output_params([[Name, _]]) :-
  write_param(Name).
output_params([[Name, _]|T]) :-
  write_param(Name),
  write(', '),
  output_params(T).
output_params([]).
output_param([]).

  
codegen(S) :-
  phrase(tokens(T), S),
  phrase(driver(Name, Params, Stmts), T),
  check_driver(Stmts, Params, A, C),
  output_code(Name, Params, C, A).

codes_to_atoms([H|T], [H2|T2]) :-
  atom_codes(H2, [H]),
  codes_to_atoms(T, T2).
codes_to_atoms([], []).
test :-
  Filename = 'test.d',
    read_file_to_string(Filename, PString, []),
    string_to_list(PString, PCodeList),
    codes_to_atoms(PCodeList, P),
    codegen(P),
    halt.

  
:- initialization main.
main :-
    current_prolog_flag(argv, [Filename]),
    read_file_to_string(Filename, PString, []),
    string_to_list(PString, PCodeList),
    codes_to_atoms(PCodeList, P),
    codegen(P),
    halt.

main :- write(compilation_error), nl, halt.

  