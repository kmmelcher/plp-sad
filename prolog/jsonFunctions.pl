:- use_module(library(http/json)).

% Lendo arquivo JSON puro
readJSON(String, File) :-
    getFilePath(String, FilePath),
    open(FilePath, read, F),
    json_read_dict(F, File).

getFilePath(String, FilePath) :-
    atom_concat("database/", String, S),
    atom_concat(S, ".json", FilePath).

    