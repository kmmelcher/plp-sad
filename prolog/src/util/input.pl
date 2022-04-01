:- module('input',[input/1]).

input(Input) :-
    read_line_to_codes(user_input, X1),
    string_to_atom(X1,Input).