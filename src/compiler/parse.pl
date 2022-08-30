:- module(parser, [driver/5]).
num(Val)  --> [Val], {number(Val)}.
addr(Addr) --> [tt_at], num(AddrNum), { atom_number(Addr, AddrNum) }.
var(Name) --> [Name], {atom(Name)}.
bitrange(Start1, End1) --> [tt_open_brace], num(Start), [tt_colon], num(End), [tt_close_brace], {Start1 is 31 - End, End1 is 31 - Start}.

stmt(write, Addr, Varname, _, _, _, _) --> addr(Addr), [tt_left_arrow], var(Varname), [tt_semicolon].
stmt(vu_param, _, Varname, Paramname, Start, End, _) --> 
  var(Varname), bitrange(Start, End), [tt_colon_equal], var(Paramname), [tt_semicolon].
stmt(vu_num, _, Varname, _, Start, End, Value) --> 
  var(Varname), bitrange(Start, End), [tt_colon_equal], num(Value), [tt_semicolon].
stmt(vd_addr, Addr, Varname, _, _, _, _) --> var(Varname), [tt_colon_equal], addr(Addr), [tt_semicolon].
stmt(vd_num, _, Varname, _, _, _, Val) --> var(Varname), [tt_colon_equal], num(Val), [tt_semicolon].
stmt(assume, Addr, _, _, Start, End, Val) -->
  addr(Addr), bitrange(Start, End), [tt_equal_equal], num(Val), [tt_semicolon].

stmts([[Action, Addr, Varname, Paramname, Start, End, Val]|Rest]) 
  --> stmt(Action, Addr, Varname, Paramname, Start, End, Val), !, stmts(Rest).
stmts([]) --> [].

param(Name, Size) --> var(Name), [tt_colon], num(Size).
param_list([[Name1, Size1] | Rest]) --> param(Name1, Size1), [tt_comma], param_list(Rest).
param_list([[Name, Size]]) --> param(Name, Size).
param_list([]) --> [].

driver(Name, Params, Stmts) --> 
  var(Name), [tt_open_paren], param_list(Params), [tt_close_paren], [tt_open_bracket], stmts(Stmts), [tt_close_bracket].

drivers([[Name, Params, Stmts]|Rest]) --> driver(Name, Params, Stmts), drivers(Rest).
drivers([[Name, Params, Stmts]]) --> driver(Name, Params, Stmts).

/* TEST:
phrase(tokens(T), "mydriver(test: 3) {
      @123[12:13] == 01;
      asdf := 132;
      asdf := @132;
      asdf[13:15] := 123;
      asdf[13:15] := asdff;
      @123 <- hello;
    }
    mydriver() {
    }
    mydriver(test: 3, test: 432) {
    }"), phrase(drivers(D), T).
*/
