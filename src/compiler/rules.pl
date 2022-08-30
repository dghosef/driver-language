must_sync(Addr) :- Addr > 538968064.

% readProperty(Addr, Val) indicates what happens when you read Addr
readProperty(_, Lastwrites) :- duplicate_list(l, 32, Lastwrites).

% List of address that get synced when Addr is read
% Our dev barrier instruction syncs everything
sync_rules(_, Addr, Synced) :-
  Addr is 15,
  range(0, 2000000, Synced).
% reads/writes within the same device are guaranteed to be ordered.
% SDRam
sync_rules(_, Addr, Synced) :-
  Addr >= 0,
  Addr =< 536870912,
  range(0, 536870912, Synced).
% GPIO
sync_rules(_, Addr, Synced) :-
  Addr >= 538968064,
  Addr =< 538968220,
  range(538968064, 538968220, Synced).
% SPI/UART
sync_rules(_, Addr, Synced) :-
  Addr >= 539054080,
  Addr =< 539054292,
  range(539054080, 539054292, Synced).
write_dependencies(539054084, [[538968068, 15, 17, [0,1,0]], [538968068, 12, 14, [0,1,0]]]).
write_dependencies(539054176, [[539054084, 0, 0, [1]]]).
write_dependencies(539054152, [[539054176, 0, 1, [0, 0]]]).
write_dependencies(539054148, [[539054176, 0, 1, [0, 0]]]).
write_dependencies(539054156, [[539054176, 0, 1, [0, 0]]]).
write_dependencies(539054184, [[539054176, 0, 1, [0, 0]]]).

write_dependencies(A, []) :- \+ write_dependencies(A, [_|_]).
% In order to write to anny address > 1000, write 010 to bits 1-3 at 123
%write_dependencies(Addr, [[123, 1, 3, [1, 0, 1]]]) :- Addr > 1000.
read_inst(15, _, Instr) :-
  Instr = [dev_barrier, '(', ')', ';'].
read_inst(Addr, VarName, Instr) :-
  Instr = [long, VarName, '=', get_32, '(', Addr, ')', ';'].

write_inst(Addr, VarName, Instr) :-
  Instr = [put_32, '(', Addr, ',', VarName, ')', ';'].