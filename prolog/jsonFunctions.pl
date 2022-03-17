:- use_module(library(http/json)).

% Fato dinâmico para gerar o id 
id(1).
incrementa_id :- retract(id(X)), Y is X + 1, assert(id(Y)).
:- dynamic id/1.

% Lendo arquivo JSON puro
readJSON(String, File) :-
    getFilePath(String, FilePath),
    open(FilePath, read, F),
    json_read_dict(F, File).

getFilePath(String, FilePath) :-
    atom_concat("database/", String, S),
    atom_concat(S, ".json", FilePath).

showRecursevily([]).
showRecursevily([Row|[]]):-
    write(Row).
showRecursevily([Row|Rows]) :-
    write(Row), write(", "), showRecursevily(Rows).

% Regras para listar todos Objetos
showObjectsAux([]):- halt.
showObjectsAux([H|T]) :- 
    write("Matricula: "), writeln(H.id),
    write("Nome: "), writeln(H.nome), 
    write("Disciplinas: "), showRecursevily(H.disciplinas), nl, nl, 
    showObjectsAux(T).

showObjects(FilePath) :-
		readJSON(FilePath, Result),
		showObjectsAux(Result).


    