:- module('chatController', [exibeTicketsDisciplina/1]).
:- use_module('../util/jsonFunctions.pl', [readJSON/2]).

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

exibeTicketsDisciplina(SiglaDisciplina):-
    getTicketsDisciplina(SiglaDisciplina, Tickets),
    (Tickets = [] -> writeln("Ainda nao ha tickets para esta disciplina."); exibeTicketsRecursivo(Tickets)).
    % TODO FALTA LER MENSAGENS DE UM TICKET

exibeTicketsRecursivo([]).
exibeTicketsRecursivo([T|Ts]):-
    swritef(Out, "%w) %w (%w)", [T.id, T.titulo, T.status]), writeln(Out),
    exibeTicketsRecursivo(Ts).