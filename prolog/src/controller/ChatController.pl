:- module('chatController', []).

:- use_module('../util/jsonFunctions.pl', [readJSON/2]).

exibeTicketsDisciplina(SiglaDisciplina):-
    readJSON("tickets", Tickets),
    exibeTicketsDisciplinaRecursivo(Tickets, SiglaDisciplina).

exibeTicketsDisciplinaRecursivo([], _).

exibeTicketsDisciplinaRecursivo([T|Ts], SiglaDisciplina):-
    T.disciplina = SiglaDisciplina,
    swritef(Out, "%w) %w (%w)", [T.id, T.titulo, T.status]), write(Out), nl,
    exibeTicketsDisciplinaRecursivo(Ts, SiglaDisciplina)    
    ;
    exibeTicketsDisciplinaRecursivo(Ts, SiglaDisciplina).