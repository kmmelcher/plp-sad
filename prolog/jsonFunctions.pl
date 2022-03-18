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

saveAlumn(String, Matricula, Nome, Disciplinas) :- 
    readJSON(String, File),
    alumnsToJSON(File, ListaAgentesJSON),
    alumnToJSON(Matricula, Nome, Disciplinas, AgenteJSON),
    append(ListaAgentesJSON, [AgenteJSON], Saida),
    getFilePath(String, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Criando representação em formato String de um agente em JSON
alumnToJSON(Matricula, Nome, Disciplinas, Out) :-
    swritef(Out, '{"id": "%w", "nome":"%w","disciplinas":"%w", "senha":""}', [Matricula, Nome, Disciplinas]).

% Convertendo uma lista de objetos em JSON para 
alumnsToJSON([], []).
alumnsToJSON([H|T], [X|Out]) :- 
    alumnToJSON(H.id, H.nome, H.disciplinas, X), 
    alumnsToJSON(T, Out).
