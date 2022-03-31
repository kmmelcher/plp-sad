:- module('ChatController', [exibeTicketsDisciplina/1, responderTicket/2, exibeTicketsAluno/1, adicionaTicket/2]).
:- use_module('../util/jsonFunctions', [readJSON/2, getObjetoByID/3, atualizaAtributoTicket/3, addMensagem/4,  addTicket/6]).
:- use_module('AlunoController.pl', [getAluno/2, ehAluno/1]).
:- use_module('MonitorController.pl', [getMonitor/2, ehMonitor/1]).
:- use_module('ProfessorController.pl', [ehProfessor/1, getProfessor/2]).


%----------------------------------------------------- FUNÇÕES DE GET -----------------------------------------------------%

getTicket(Id, Ticket):-
    getObjetoByID("tickets", Id, TicketJson),
    split_string(TicketJson.mensagens, ",", "", MensagensFormated),
    put_dict([mensagens=MensagensFormated], TicketJson, Ticket).

getMensagem(Id, Mensagem):- getObjetoByID("mensagens", Id, Mensagem).

getTicketsAluno(Matricula, Saida):-
    readJSON("tickets", TodosTickets),
    getTicketsDoAlunoRecursivo(TodosTickets, Matricula, Saida).

getTicketsDoAlunoRecursivo([],_, []).
getTicketsDoAlunoRecursivo([H|T],Matricula, Tickets):-
    H.autor = Matricula,
    getTicketsDoAlunoRecursivo(T, Matricula, TicketsS),
    append(TicketsS, [H], Tickets)
    ;
    getTicketsDoAlunoRecursivo(T, Matricula, Tickets).

getTicketsDisciplina(SiglaDisciplina, TicketsDisciplina):-
    readJSON("tickets", Tickets),
    getTicketsDisciplinaRecursivo(Tickets, SiglaDisciplina, [], TicketsDisciplina).

getTicketsDisciplinaRecursivo([], _, TicketsAux, TicketsDisciplina):- TicketsDisciplina = TicketsAux.
getTicketsDisciplinaRecursivo([T|Ts], SiglaDisciplina, TicketsAux, TicketsDisciplina):-
    T.disciplina = SiglaDisciplina,
    append(TicketsAux, [T], NovosTicketsDisciplina),
    getTicketsDisciplinaRecursivo(Ts, SiglaDisciplina, NovosTicketsDisciplina, TicketsDisciplina)
    ;
    getTicketsDisciplinaRecursivo(Ts, SiglaDisciplina, TicketsAux, TicketsDisciplina).

getTicketsEmAndamento(Disciplina, TicketsEmAndamento):-
    getTicketsDisciplina(Disciplina, Tickets),
    getTicketsEmAndamentoRecursivo(Tickets, [], TicketsEmAndamento).

getTicketsEmAndamentoRecursivo([], TicketsAux, TicketsDisciplina):- TicketsDisciplina = TicketsAux.
getTicketsEmAndamentoRecursivo([T|Ts], TicketsAux, TicketsEmAndamento):-
    T.status = "Em andamento",
    append(TicketsAux, [T], NovosTickets),
    getTicketsEmAndamentoRecursivo(Ts, NovosTickets, TicketsEmAndamento);
    getTicketsEmAndamentoRecursivo(Ts, TicketsAux, TicketsEmAndamento).

%----------------------------------------------------- FUNÇÕES DE EXIBIÇÃO -----------------------------------------------------%
exibirTickets([]).
exibirTickets([H|T]):-
    swritef(Out, "%w) %w (%w)", [H.id, H.titulo, H.status]), write(Out), nl,
    exibirTickets(T).

exibeTicketsAluno(Matricula):-
    getTicketsAluno(Matricula, Tickets),
    (Tickets = [] -> writeln("Voce ainda nao criou nenhum ticket."); exibirTickets(Tickets)).

exibeTicketsDisciplina(SiglaDisciplina):-
    swritef(Out, "Estes sao os tickets da disciplina: %w\n", [SiglaDisciplina]), write(Out),
    getTicketsDisciplina(SiglaDisciplina, Tickets),
    (
        Tickets = [] -> writeln("Ainda nao ha tickets para esta disciplina.")
        ; 
        exibirTickets(Tickets),
        writeln("\n Qual o numero do ticket que deseja visualizar mensagens? "), 
        read(Opcao),
        atom_string(Opcao, OpcaoStr),
        (
            checaIdValidoEmTickets(OpcaoStr, Tickets) -> getTicket(OpcaoStr, Ticket), exibeMensagensTicket(Ticket.mensagens, Ticket.disciplina); 
            msgInputInvalido()
        )
    ).

checaIdValidoEmTickets(_, []):- false.
checaIdValidoEmTickets(Opcao, [T|Ts]):- Opcao = T.id ; checaIdValidoEmTickets(Opcao, Ts).

exibeMensagensTicket([], _).
exibeMensagensTicket([Im|Ims], Disciplina):-
    getMensagem(Im, Mensagem),
    checaEntidadeParaMensagem(Mensagem.autor, Disciplina, Autor, Entidade),
    swritef(Out, "[%w] (%w) %w - %w\n", [Mensagem.horario, Autor, Entidade.nome, Mensagem.conteudo]), write(Out),
    exibeMensagensTicket(Ims, Disciplina).


responderTicket(Entidade, Disciplina):-
    getTicketsEmAndamento(Disciplina, Tickets),
    (
      Tickets = [] -> writeln("Ainda nao ha tickets em andamento para serem respondidos nesta disciplina.\n") ;
      swritef(Out, "Estes sao os tickets em andamento da disciplina: %w\n\n", [Disciplina]), write(Out),
        exibirTickets(Tickets),
        writeln("Qual ticket voce deseja responder? "), read(Opcao), atom_string(Opcao, OpcaoStr),
        (
            checaIdValidoEmTickets(OpcaoStr, Tickets) -> getTicket(OpcaoStr, Ticket), adicionaMensagem(Entidade, Ticket);
            msgInputInvalido()
        )
    ).

adicionaMensagem(Entidade, Ticket):-
    checaEntidadeParaMensagem(Entidade.id, Ticket.disciplina, Autor, _),
    exibeMensagensTicket(Ticket.mensagens, Ticket.disciplina),
    writeln("Insira a mensagem entre aspas simples: "), read(Conteudo),
    get_time(T), format_time(string(Horario), "%c", T),
    addMensagem(Entidade.id, Conteudo, Horario, IdMensagem),
    atualizaAtributoTicket(Ticket.id, "mensagens", IdMensagem),   
    writeln("Mensagem adicionada com sucesso.\n").

checaEntidadeParaMensagem(Id, Disciplina, Autor, Entidade):-
    ehMonitor(Id), getMonitor(Id, Monitor), Monitor.disciplina = Disciplina -> Autor = "MONITOR", getAluno(Id, Entidade);
    ehProfessor(Id) -> Autor = "PROFESSOR", getProfessor(Id, Entidade);
    Autor = "ALUNO", getAluno(Id, Entidade).

msgInputInvalido():- writeln("Insira um valor valido\n").

adicionaTicket(Aluno, Disciplina):-
    writeln("Insira o titulo do seu ticket: "),
    read(Titulo),
    addTicket("", Titulo, Aluno.id, [""], "Em andamento", Disciplina).