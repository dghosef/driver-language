%  phrase(tokens(T), "mydriver(param: 4, params[D: 3) { @123[2:4]} == 123142} asdf := 214 asdf := @124 asdf[124:124] := 124; fd[1:3] := asdf; @14 <- 34").
:- module(lexer, [tokens/3]).
:- set_prolog_flag(double_quotes, chars).

whitespace   --> [A], { char_type(A, white) }.
whitespace   --> [A], { char_type(A, newline) }.
whitespaces  --> whitespace, whitespaces.
whitespaces  --> "".

% @, {, }, [, ], (, ), :=, ==, ;, <-
symbol(tt_at)             --> whitespaces, "@".
symbol(tt_open_bracket)   --> whitespaces, "{".
symbol(tt_close_bracket)  --> whitespaces, "}".
symbol(tt_open_brace)     --> whitespaces, "[".
symbol(tt_close_brace)    --> whitespaces, "]".
symbol(tt_open_paren)     --> whitespaces, "(".
symbol(tt_close_paren)    --> whitespaces, ")".
symbol(tt_colon_equal)    --> whitespaces, ":=".
symbol(tt_equal_equal)    --> whitespaces, "==".
symbol(tt_left_arrow)     --> whitespaces, "<-".
symbol(tt_semicolon)      --> whitespaces, ";".
symbol(tt_colon)          --> whitespaces, ":".
symbol(tt_comma)          --> whitespaces, ",".

letter(D)                     --> [D], { char_type(D, alpha) }.
letters(Res)                  --> letter(D), letters(D1), {atom_concat(D, D1, Res)}.
letters(D)                    --> letter(D).
identifier(tt_identifier, D)  --> whitespaces, letters(D).

digit(D)                    --> [D], { char_type(D, digit) }.
digits(Res)                 --> digit(D), digits(D1), {atom_concat(D, D1, Res)}.
digits(D)                   --> digit(D).
number(tt_int_literal, Res) --> whitespaces, digits(D), {atom_number(D, Res)}.

tokens([Sym|Rest]) --> symbol(Sym), !, tokens(Rest).
tokens([Num|Rest]) --> number(_, Num), !, tokens(Rest).
tokens([Id|Rest])  --> identifier(_, Id), !, tokens(Rest).
tokens([])         --> whitespaces.