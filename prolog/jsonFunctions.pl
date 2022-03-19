:- use_module(library(http/json)).

getIDs([], ListaIDs, Result):- append([0], ListaIDs, Result).
getIDs([H|T], ListaIDs, Result):-
    atom_number(H.id, X),
    append([X], ListaIDs, Result),
    getIDs(T,Result, Aux).

max([Max], Max).
max([Head | List], Max) :-
  max(List, MaxList),
  ( Head > MaxList -> Max = Head ; Max = MaxList ).

buscaNovoID(NomeArquivo, ID):-
    readJSON(NomeArquivo, File),
    getIDs(File, [], Result),
    max(Result, M),
    ID is M + 1.

% Lendo arquivo JSON puro
readJSON(NomeArquivo, File) :-
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, read, F),
    (at_end_of_stream(F) -> File = [] ; json_read_dict(F, File)). 
    
getFilePath(NomeArquivo, FilePath) :-
    atom_concat("database/", NomeArquivo, S),
    atom_concat(S, ".json", FilePath).

% Usado para mostrar conteúdo de listas
showRecursevily([]).
showRecursevily([Row|[]]):-
    write(Row).
showRecursevily([Row|Rows]) :-
    write(Row), write(", "), showRecursevily(Rows).

% Removendo 
removeObjectJSON([], _, []).
removeObjectJSON([H|T], H.id, T).
removeObjectJSON([H|T], Id, [H|Out]) :- removeObjectJSON(T, Id, Out).

getObjetoRecursivamente([], _, "").
getObjetoRecursivamente([H|T], Id, Out):-
     (H.id = Id -> Out = H);(getObjetoRecursivamente(T, Id, Out)).

% getObjetoById(NomeArquivo, Id, Out):-
%     readJSON(NomeArquivo, File),
%     getObjetoRecursivamente(File, Id, Out). 

getObjetoByID(NomeArquivo, Id, Result):-
    readJSON(NomeArquivo, File),
    getObjetoRecursivamente(File, Id, Result).

%-------------------------- Funções de Alunos--------------------------%

showAlunosAux([]):- halt.
showAlunosAux([H|T]) :- 
    write("Matricula: "), writeln(H.id),
    write("Nome: "), writeln(H.nome), 
    write("Disciplinas: "), showRecursevily(H.disciplinas), nl, nl, 
    showAlunosAux(T).

showAlunos() :-
		readJSON("alunos", Result),
		showAlunosAux(Result).

addAluno(Matricula, Nome, Disciplinas) :- 
    NomeArquivo = "alunos",
    readJSON(NomeArquivo, File),
    alunosToJSON(File, ListaObjectsJSON),
    alunoToJSON(Matricula, Nome, Disciplinas, ObjectJSON),
    append(ListaObjectsJSON, [ObjectJSON], Saida),
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Usado para alunos 
alunoToJSON(Id, Nome, Disciplinas, Out) :-
    swritef(Out, '{"id":"%w", "nome":"%w","disciplinas":"%w", "senha":""}', [Id, Nome, Disciplinas]).

% Convertendo uma lista de objetos em JSON para 
alunosToJSON([], []).
alunosToJSON([H|T], [X|Out]) :- 
    alunoToJSON(H.id, H.nome, H.disciplinas, X), 
    alunosToJSON(T, Out).

removeAluno(NomeArquivo, Id) :-
   readJSON(NomeArquivo, File),
   removeObjectJSON(File, Id, SaidaParcial),
   alunosToJSON(SaidaParcial, Saida),
   getFilePath(NomeArquivo, FilePath),
   open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

%-------------------------- Funções de Professores--------------------------%
 
showProfessoresAux([]):- halt.
showProfessoresAux([H|T]) :- 
    write("Id: "), writeln(H.id),
    write("Nome: "), writeln(H.nome), 
    write("Disciplinas: "), showRecursevily(H.disciplinas), nl, nl, 
    showProfessoresAux(T).

showProfessores() :-
		readJSON("professores", Result),
		showProfessoresAux(Result).

professorToJSON(Id, Nome, Disciplinas, Out) :-
    swritef(Out, '{"id": "%w", "nome":"%w","disciplinas":"%w", "senha":""}', [Id, Nome, Disciplinas]).

professoresToJSON([], []).
professoresToJSON([H|T], [X|Out]) :- 
    professorToJSON(H.id, H.nome, H.disciplinas, X), 
    professoresToJSON(T, Out).

addProfessor(Nome, Disciplinas) :- 
    NomeArquivo = "professores",
    buscaNovoID(NomeArquivo, ID),
    readJSON(NomeArquivo, File),
    professoresToJSON(File, ListaObjectsJSON),
    professorToJSON(ID, Nome, Disciplinas, ObjectJSON),
    append(ListaObjectsJSON, [ObjectJSON], Saida),
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

%-------------------------- Funções de Monitores--------------------------%

showMonitoresAux([]):- halt.
showMonitoresAux([H|T]) :- 
    write("Id: "), writeln(H.id),
    write("Nome: "), writeln(H.nome), 
    write("Disciplinas: "), showRecursevily(H.disciplinas), nl,
    write("Horários: "), showRecursevily(H.horarios),  nl, 
    showMonitoresAux(T).

showMonitores() :-
		readJSON("monitores", Result),
		showMonitoresAux(Result).

monitorToJSON(Id, Nome, Disciplinas, Horarios, Out) :-
    swritef(Out, '{"id": "%w", "nome":"%w","disciplinas":"%w", "horarios":"%w" "senha":""}', [Id, Nome, Disciplinas, Horarios]).

monitoresToJSON([], []).
monitoresToJSON([H|T], [X|Out]) :- 
    monitorToJSON(H.id, H.nome, H.disciplinas, H.horarios, X), 
    monitoresToJSON(T, Out).

addMonitor(Nome, Disciplinas, Horarios) :- 
    NomeArquivo = "monitores",
    buscaNovoID(NomeArquivo, ID),
    readJSON(NomeArquivo, File),
    monitoresToJSON(File, ListaObjectsJSON),
    monitorToJSON(ID, Nome, Disciplinas, Horarios, ObjectJSON),
    append(ListaObjectsJSON, [ObjectJSON], Saida),
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

removeMonitor(NomeArquivo, Id) :-
    readJSON(NomeArquivo, File),
    removeObjectJSON(File, Id, SaidaParcial),
    monitoresToJSON(SaidaParcial, Saida),
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).
