:- module('jsonFunctions', [
    addAluno/4,
    addMonitor/3, 
    removeMonitor/1, 
    existeDisciplina/1, 
    readJSON/2, 
    checaExistencia/2, 
    getObjetoByID/3, 
    atualizaAtributoAluno/3, 
    buscaNovoID/2, 
    addMensagem/4, 
    atualizaAtributoProfessor/3,
    atualizaAtributoTicket/3
    ]).

:- use_module(library(http/json)).
:- use_module('../controller/AlunoController.pl', [getAluno/2]).

stringlist_concat([H|[]], _, ResultAux, ResultReal):- string_concat(ResultAux, H, ResultReal).
stringlist_concat([H|T], Sep, ResultAux, ResultReal):-
    string_concat(H, Sep, NovoResultAux),
    string_concat(ResultAux,NovoResultAux, Result),
    stringlist_concat(T, Sep, Result, ResultReal).


getIDs([], ListaIDs, Result):- append([0], ListaIDs, Result).
getIDs([H|T], ListaIDs, Result):-
    atom_number(H.id, X),
    getIDs(T,ListaIDs, ResultAux),
    append([X], ResultAux, Result).

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
showRecursively([]).
showRecursively([Row|[]]):-
    write(Row).
showRecursively([Row|Rows]) :-
    write(Row), write(", "), showRecursively(Rows).

% Removendo 
removeObjectJSON([], _, []).
removeObjectJSON([H|T], H.id, T).
removeObjectJSON([H|T], Id, [H|Out]) :- removeObjectJSON(T, Id, Out).

getObjetoRecursivamente([], _, "").
getObjetoRecursivamente([H|T], Id, Out):-
     (H.id = Id -> Out = H);(getObjetoRecursivamente(T, Id, Out)).

getObjetoByID(NomeArquivo, Id, Result):-
    readJSON(NomeArquivo, File),
    getObjetoRecursivamente(File, Id, Result).

checaExistencia(NomeArquivo, Id):-
    readJSON(NomeArquivo, File),
    atom_string(Id, IdString),
    getObjetoRecursivamente(File, IdString, Result),
    Result \= "".

%-------------------------- Funções de Alunos--------------------------%
showAlunosAux([]):- !.
showAlunosAux([H|T]) :- 
    write("Matricula: "), writeln(H.id),
    write("Nome: "), writeln(H.nome),
    split_string(H.disciplinas, ",", "", Disciplinas), 
    write("Disciplinas: "), showRecursively(Disciplinas), nl, nl, 
    showAlunosAux(T).

showAlunos() :-
		readJSON("alunos", Result),
		showAlunosAux(Result).

showAluno(Id) :-
    getObjetoByID("alunos", Id, H),
    write("Matricula: "), write(H.id),
    write(" | Nome: "), write(H.nome),
    split_string(H.disciplinas, ",", "", Disciplinas),
    write(" | Disciplinas: "), showRecursively(Disciplinas), nl, nl.

atualizaAtributoAluno(Id, Atributo, ConteudoAtualizado):-
    getObjetoByID("alunos", Id, Object),
    split_string(Object.disciplinas, ",", "", ConteudoAux),
    (Atributo = "nome" ->  removeAluno(Id), addAluno(Object.id, ConteudoAtualizado, Object.disciplinas, Object.senha); 
     Atributo = "disciplinas" -> append(ConteudoAux, [ConteudoAtualizado], NovoConteudo), removeAluno(Id), addAluno(Object.id, Object.nome, NovoConteudo, Object.senha);
     Atributo = "senha" -> removeAluno(Id), addAluno(Object.id, Object.nome, Object.disciplinas, ConteudoAtualizado)).

addAluno(Matricula, Nome, Disciplinas, Senha) :- 
    NomeArquivo = "alunos",
    readJSON(NomeArquivo, File),
    alunosToJSON(File, ListaObjectsJSON),
    stringlist_concat(Disciplinas, ",", "", DisciplinasFormated),
    alunoToJSON(Matricula, Nome, DisciplinasFormated, Senha, ObjectJSON),
    append(ListaObjectsJSON, [ObjectJSON], Saida),
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Usado para alunos 
alunoToJSON(Id, Nome, Disciplinas, Senha, Out) :-
    swritef(Out, '{"id":"%w", "nome":"%w","disciplinas":"%w", "senha":"%w"}', [Id, Nome, Disciplinas,Senha]).

% Convertendo uma lista de objetos em JSON para 
alunosToJSON([], []).
alunosToJSON([H|T], [X|Out]) :- 
    alunoToJSON(H.id, H.nome, H.disciplinas, H.senha, X), 
    alunosToJSON(T, Out).

removeAluno(Id) :-
    NomeArquivo = "alunos",
    readJSON(NomeArquivo, File),
    removeObjectJSON(File, Id, SaidaParcial),
    alunosToJSON(SaidaParcial, Saida),
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

%-------------------------- Funções de Professores--------------------------%
 
showProfessoresAux([]):- !.

showProfessoresAux([H|T]) :- 
    write("Id: "), writeln(H.id),
    write("Nome: "), writeln(H.nome),
    split_string(H.disciplinas, ",", "", Disciplinas), 
    write("Disciplinas: "), showRecursively(Disciplinas), nl, nl, 
    showProfessoresAux(T).

showProfessores() :-
		readJSON("professores", Result),
		showProfessoresAux(Result).

professorToJSON(Id, Nome, Disciplinas, Senha, Out) :-
    swritef(Out, '{"id": "%w", "nome":"%w","disciplinas":"%w", "senha":"%w"}', [Id, Nome, Disciplinas,Senha]).

professoresToJSON([], []).
professoresToJSON([H|T], [X|Out]) :- 
    professorToJSON(H.id, H.nome, H.disciplinas, H.senha, X), 
    professoresToJSON(T, Out).

atualizaAtributoProfessor(Id, Atributo, ConteudoAtualizado):-
    getObjetoByID("professores", Id, Object),
    (Atributo = "nome" ->  removeProfessor(Id), addProfessor(Object.id, ConteudoAtualizado, Object.disciplinas, Object.senha); 
     Atributo = "disciplinas" -> removeProfessor(Id), addProfessor(Object.id, Object.nome, ConteudoAtualizado, Object.senha);
     Atributo = "senha" -> removeProfessor(Id), addProfessor(Object.id, Object.nome, Object.disciplinas, ConteudoAtualizado)).

removeProfessor(Id) :-
    NomeArquivo = "professores",
    readJSON(NomeArquivo, File),
    removeObjectJSON(File, Id, SaidaParcial),
    professoresToJSON(SaidaParcial, Saida),
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

addProfessor(Matricula, Nome, Disciplinas, Senha) :- 
    NomeArquivo = "professores",
    readJSON(NomeArquivo, File),
    professoresToJSON(File, ListaObjectsJSON),
    professorToJSON(Matricula, Nome, Disciplinas, Senha, ObjectJSON),
    append(ListaObjectsJSON, [ObjectJSON], Saida),
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

%-------------------------- Funções de Monitores--------------------------%

showMonitoresAux([]).
showMonitoresAux([H|T]) :-
    getAluno(H.id, Aluno),
    write("Matrícula: "), writeln(H.id),
    write("Nome: "), writeln(Aluno.nome),
    write("Horários: "), writeln(H.horarios),
    nl,
    showMonitoresAux(T).

showMonitores() :-
    readJSON("monitores", Result),
    showMonitoresAux(Result).

showMonitor(Id) :-
    getObjetoByID("monitores", Id, H),
    write("Matricula: "), write(H.id),
    write("Nome: "), writeln(H.nome), 
    split_string(H.disciplinas, ",", "", Disciplinas),
    write("Disciplinas: "), showRecursively(Disciplinas), nl,
    write("Horários: "), showRecursively(H.horarios),  nl.

atualizaAtributoMonitor(Id, Atributo, ConteudoAtualizado):-
    getObjetoByID("monitores", Id, Object),
    split_string(Object.disciplinas, ",", "", ConteudoAux),
    (Atributo = "disciplina" ->  append(ConteudoAux, [ConteudoAtualizado], NovoConteudo), removeMonitor(Id), addMonitor(Object.id, NovoConteudo, Object.horarios);
     Atributo = "horarios" -> removeMonitor(Id), addMonitor(Object.id, Object.disciplina, ConteudoAtualizado)).

monitorToJSON(Id, Disciplina, Horarios, Out) :-
    swritef(Out, '{"id":"%w","disciplina":"%w","horarios":"%w"}', [Id, Disciplina, Horarios]).

monitoresToJSON([], []).
monitoresToJSON([H|T], [X|Out]) :- 
    monitorToJSON(H.id, H.disciplinas, H.horarios, X), 
    monitoresToJSON(T, Out).

addMonitor(Matricula, Disciplinas, Horarios) :- 
    NomeArquivo = "monitores",
    readJSON(NomeArquivo, File),
    monitoresToJSON(File, ListaObjectsJSON),
    stringlist_concat(Disciplinas, ",", "", DisciplinasFormated),
    monitorToJSON(Matricula, DisciplinasFormated, Horarios, ObjectJSON),
    append(ListaObjectsJSON, [ObjectJSON], Saida),
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

removeMonitor(Id) :-
    NomeArquivo = "monitores",
    readJSON(NomeArquivo, File),
    removeObjectJSON(File, Id, SaidaParcial),
    monitoresToJSON(SaidaParcial, Saida),
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

%-------------------------- Funções de Ticket--------------------------%

showTicketAux([]):- !.
showTicketAux([H|T]) :- 
    write("Matricula: "), writeln(H.id),
    write("Nome: "), writeln(H.nome),
    write("Disciplinas: "), showRecursively(H.disciplinas), nl,
    write("Horários: "), showRecursively(H.horarios),  nl, 
    showTicketAux(T).

showTicket() :-
    readJSON("tickets", Result),
    showTicketAux(Result).

showTicket(Id) :-
    getObjetoByID("tickets", Id, H),
    write("Id: "), writeln(H.id),
    write("Titulo: "), writeln(H.titulo), 
    write("Autor: "), writeln(H.autor), 
    write("Mensagens: "), showRecursively(H.mensagens), nl,
    write("Status: "), writeln(H.status),
    write("Disciplina: "), writeln(H.disciplina).

ticketToJSON(ID, Titulo, Autor, Mensagens, Status, Disciplina, Out) :-
    swritef(Out, '{"id":"%w", "titulo":"%w","autor":"%w","mensagens":"%w","status":"%w","disciplina":"%w"}', [ID, Titulo, Autor, Mensagens, Status, Disciplina]).

ticketToJSON([], []).
ticketToJSON([H|T], [X|Out]) :- 
    ticketToJSON(H.id, H.titulo,H.autor,H.mensagens, H.status,H.disciplina, X), 
    ticketToJSON(T, Out).

%Quando criar um novo ticket passar como -1
addTicket(ID, Titulo, Autor, Mensagens, Status, Disciplina) :- 
    NomeArquivo = "tickets",
    (ID =:= -1 -> buscaNovoID(NomeArquivo, IDAux); IDAux = ID),
    readJSON(NomeArquivo, File),
    ticketToJSON(File, ListaObjectsJSON),
    stringlist_concat(Mensagens, ",", "", MensagensFormated),
    ticketToJSON(IDAux, Titulo, Autor, MensagensFormated, Status, Disciplina, ObjectJSON),
    append(ListaObjectsJSON, [ObjectJSON], Saida),
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

removeTicket(Id) :-
    NomeArquivo = "tickets",
    readJSON(NomeArquivo, File),
    removeObjectJSON(File, Id, SaidaParcial),
    ticketToJSON(SaidaParcial, Saida),
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

atualizaAtributoTicket(Id, Atributo, ConteudoAtualizado):-
    getObjetoByID("tickets", Id, Object),
    split_string(Object.mensagens, ",", "", ConteudoAux),
    (Atributo = "mensagens" ->  append(ConteudoAux, [ConteudoAtualizado], NovoConteudo), removeTicket(Id), addTicket(Object.id, Object.titulo, Object.autor, NovoConteudo, Object.status, Object.disciplina); 
    Atributo = "status" -> removeTicket(Id), addTicket(Object.id, Object.titulo, Object.autor, Object.mensagens, ConteudoAtualizado, Object.disciplina)).

%-------------------------- Funções de Mensagens --------------------------%

showMensagensAux([]):- !.
showMensagensAux([H|T]) :- 
    write("Matricula: "), writeln(H.id),
    write("Nome: "), writeln(H.nome), 
    write("Disciplinas: "), showRecursively(H.disciplinas), nl,
    write("Horários: "), showRecursively(H.horarios),  nl, 
    showMensagensAux(T).

showMensagens() :-
    readJSON("mensagens", Result),
    showMensagensAux(Result).

showMensagens(Id) :-
    getObjetoByID("mensagens", Id, H),
    write("Id: "), writeln(H.id),
    write("Titulo: "), writeln(H.titulo), 
    write("Autor: "), writeln(H.autor), 
    write("Mensagens: "), showRecursively(H.mensagens), nl,
    write("Status: "), writeln(H.status),
    write("Disciplina: "), writeln(H.disciplina).

mensagemToJSON(ID, Autor, Conteudo, Horario, Out) :-
    swritef(Out, '{"id":"%w","autor":"%w","conteudo":"%w","horario":"%w"}', [ID, Autor, Conteudo, Horario]).

mensagensToJSON([], []).
mensagensToJSON([H|T], [X|Out]) :- 
    mensagemToJSON(H.id, H.autor,H.conteudo, H.horario, X), 
    mensagensToJSON(T, Out).

addMensagem(Autor, Conteudo, Horario, ID) :- 
    NomeArquivo = "mensagens",
    buscaNovoID(NomeArquivo, ID),
    readJSON(NomeArquivo, File),
    mensagensToJSON(File, ListaObjectsJSON),
    mensagemToJSON(ID, Autor, Conteudo, Horario, ObjectJSON),
    append(ListaObjectsJSON, [ObjectJSON], Saida),
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

removeMensagem(Id) :-
    NomeArquivo = "mensagens",
    readJSON(NomeArquivo, File),
    removeObjectJSON(File, Id, SaidaParcial),
    mensagensToJSON(SaidaParcial, Saida),
    getFilePath(NomeArquivo, FilePath),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

%-------------------------- Funções de Disciplinas --------------------------%

getDisciplinaRecursivamente([], _, "").
getDisciplinaRecursivamente([H|T], Sigla, Out):-
     (H.sigla = Sigla -> Out = H);(getDisciplinaRecursivamente(T, Sigla, Out)).

getDisciplinaBySigla(Disciplina, Result):-
    readJSON("disciplinas", File),
    getDisciplinaRecursivamente(File, Disciplina, Result).

existeDisciplina(Sigla):-
    readJSON("disciplinas", File),
    atom_string(Sigla, SiglaStr),
    getDisciplinaRecursivamente(File, SiglaStr, Result),
    Result \= "".
