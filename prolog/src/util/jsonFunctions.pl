:- use_module(library(http/json)).

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

getObjetoByID(NomeArquivo, Id, Result):-
    readJSON(NomeArquivo, File),
    getObjetoRecursivamente(File, Id, Result).

checaExistencia(NomeArquivo, Id):-
    readJSON(NomeArquivo, File),
    atom_string(Id, IdString),
    getObjetoRecursivamente(File, IdString, Result),
    Result \= "".

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

showAluno(Id) :-
    getObjetoByID("alunos", Id, H),
    write("Matricula: "), write(H.id),
    write(" | Nome: "), write(H.nome), 
    write(" | Disciplinas: "), showRecursevily(H.disciplinas), nl, nl.

atualizaAtributoAluno(Id, Atributo, ConteudoAtualizado):-
    getObjetoByID("alunos", Id, Object),
    (Atributo = "nome" ->  removeAluno(Id), addAluno(Object.id, ConteudoAtualizado, Object.disciplinas, Object.senha); 
     Atributo = "disciplinas" -> removeAluno(Id), addAluno(Object.id, Object.nome, ConteudoAtualizado, Object.senha);
     Atributo = "Senha" -> removeAluno(Id), addAluno(Object.id, Object.nome, Object.disciplinas, ConteudoAtualizado)).

addAluno(Matricula, Nome, Disciplinas, Senha) :- 
    NomeArquivo = "alunos",
    readJSON(NomeArquivo, File),
    alunosToJSON(File, ListaObjectsJSON),
    alunoToJSON(Matricula, Nome, Disciplinas, Senha, ObjectJSON),
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
 
showProfessoresAux([]):- halt.
showProfessoresAux([H|T]) :- 
    write("Id: "), writeln(H.id),
    write("Nome: "), writeln(H.nome), 
    write("Disciplinas: "), showRecursevily(H.disciplinas), nl, nl, 
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

%-------------------------- Funções de Monitores--------------------------%

showMonitoresAux([]):- halt.
showMonitoresAux([H|T]) :- 
    write("Matricula: "), writeln(H.id),
    write("Disciplina: "), showRecursevily(H.disciplinas), nl,
    write("Horários: "), showRecursevily(H.horarios),  nl, 
    showMonitoresAux(T).

showMonitores() :-
    readJSON("monitores", Result),
    showMonitoresAux(Result).

showMonitor(Id) :-
    getObjetoByID("monitores", Id, H),
    write("Matricula: "), write(H.id),
    write("Disciplina: "), showRecursevily(H.disciplinas), nl,
    write("Horários: "), showRecursevily(H.horarios),  nl.

atualizaAtributoMonitor(Id, Atributo, ConteudoAtualizado):-
    getObjetoByID("monitores", Id, Object),
    (Atributo = "disciplinas" -> removeMonitor(Id), addMonitor(Object.id, ConteudoAtualizado, Object.horario);
     Atributo = "horarios" -> removeMonitor(Id), addMonitor(Object.id, Object.disciplina, ConteudoAtualizado)).

monitorToJSON(Id, Disciplina, Horarios, Out) :-
    swritef(Out, '{"id":"%w","disciplina":"%w","horarios":"%w"}', [Id, Disciplina, Horarios]).

monitoresToJSON([], []).
monitoresToJSON([H|T], [X|Out]) :- 
    monitorToJSON(H.id, H.disciplina, H.horarios, X), 
    monitoresToJSON(T, Out).

addMonitor(Matricula, Disciplina, Horarios) :- 
    NomeArquivo = "monitores",
    readJSON(NomeArquivo, File),
    monitoresToJSON(File, ListaObjectsJSON),
    monitorToJSON(Matricula, Disciplina, Horarios, ObjectJSON),
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

showTicketAux([]):- halt.
showTicketAux([H|T]) :- 
    write("Matricula: "), writeln(H.id),
    write("Nome: "), writeln(H.nome), 
    write("Disciplinas: "), showRecursevily(H.disciplinas), nl,
    write("Horários: "), showRecursevily(H.horarios),  nl, 
    showTicketAux(T).

showTicket() :-
    readJSON("tickets", Result),
    showTicketAux(Result).

showTicket(Id) :-
    getObjetoByID("tickets", Id, H),
    write("Id: "), writeln(H.id),
    write("Titulo: "), writeln(H.titulo), 
    write("Autor: "), writeln(H.autor), 
    write("Mensagens: "), showRecursevily(H.mensagens), nl,
    write("Status: "), writeln(H.status),
    write("Disciplina: "), writeln(H.disciplina).

ticketToJSON(ID, Titulo, Autor, Mensagens, Status, Disciplina, Out) :-
    swritef(Out, '{"id":"%w", "titulo":"%w","autor":"%w","mensagens":"%w","status":"%w","disciplina":"%w","senha":""}', [ID, Titulo, Autor, Mensagens, Status, Disciplina]).

ticketToJSON([], []).
ticketToJSON([H|T], [X|Out]) :- 
    ticketToJSON(H.id, H.titulo,H.autor,H.mensagens, H.status,H.disciplina, X), 
    ticketToJSON(T, Out).

addTicket(Titulo, Autor, Mensagens, Status, Disciplina) :- 
    NomeArquivo = "tickets",
    buscaNovoID(NomeArquivo, ID),
    readJSON(NomeArquivo, File),
    ticketToJSON(File, ListaObjectsJSON),
    ticketToJSON(ID, Titulo, Autor, Mensagens, Status, Disciplina, ObjectJSON),
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

%-------------------------- Funções de Mensagens --------------------------%

showMensagensAux([]):- halt.
showMensagensAux([H|T]) :- 
    write("Matricula: "), writeln(H.id),
    write("Nome: "), writeln(H.nome), 
    write("Disciplinas: "), showRecursevily(H.disciplinas), nl,
    write("Horários: "), showRecursevily(H.horarios),  nl, 
    showMensagensAux(T).

showMensagens() :-
    readJSON("mensagens", Result),
    showMensagensAux(Result).

showMensagens(Id) :-
    getObjetoByID("mensagens", Id, H),
    write("Id: "), writeln(H.id),
    write("Titulo: "), writeln(H.titulo), 
    write("Autor: "), writeln(H.autor), 
    write("Mensagens: "), showRecursevily(H.mensagens), nl,
    write("Status: "), writeln(H.status),
    write("Disciplina: "), writeln(H.disciplina).

mensagemToJSON(ID, Autor, Conteudo, Horario, Out) :-
    swritef(Out, '{"id":"%w","autor":"%w","conteudo":"%w","horario":"%w","senha":""}', [ID, Autor, Conteudo, Horario]).

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
     (H.sigla = Sigla -> Out = H);(getObjetoRecursivamente(T, Sigla, Out)).

getDisciplinaBySigla(Disciplina, Result):-
    readJSON("disciplinas", File),
    getDisciplinaRecursivamente(File, Disciplina, Result).