module('input',[pegaInput/0]).

pegaInput :-
read_line_to_codes(user_input, X2),
string_to_atom(X2,X1),
atom_number(X1,X),
write(Y).