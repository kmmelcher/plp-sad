:- module('ChatController', [exibeTicketsDisciplina/1, exibeTicketsAluno/1, getTicketsAluno/2]).
:- use_module('../util/jsonFunctions', [readJSON/2]).

getTicket(Id, Ticket):-
    getObjetoByID("tickets", Id, TicketJson),
    split_string(TicketJson.mensagens, ",", "", MensagensFormated),
    put_dict([mensagens=MensagensFormated], TicketJson, Ticket).

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

exibirTickets([]).
exibirTickets([H|T]):-
    swritef(Out, "%w) %w (%w)", [H.id, H.titulo, H.status]), write(Out), nl,
    exibirTickets(T).

exibeTicketsAluno(Matricula):-
    getTicketsAluno(Matricula, Tickets),
    (Tickets = [] -> writeln("Voce ainda nao criou nenhum ticket."); exibirTickets(Tickets)).

exibeTicketsDisciplina(SiglaDisciplina):-
    getTicketsDisciplina(SiglaDisciplina, Tickets),
    (Tickets = [] -> writeln("Ainda nao ha tickets para esta disciplina."); exibirTickets(Tickets)).
    % TODO FALTA LER MENSAGENS DE UM TICKET
