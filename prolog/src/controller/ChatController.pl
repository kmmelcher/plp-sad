:- module('chatController', []).

:- use_module('../util/jsonFunctions.pl', [readJSON/2]).

exibeTicketsDisciplina():-
    writeln("Insira a disciplina:"),
    read(SiglaDisciplina),
    atom_string(SiglaDisciplina, SiglaString),
    readJSON("tickets", Tickets),
    exibeTicketsDisciplinaRecursivo(Tickets, SiglaString).

exibeTicketsDisciplinaRecursivo([], _).

exibeTicketsDisciplinaRecursivo([T|Ts], SiglaDisciplina):-
    T.disciplina = SiglaDisciplina,
    swritef(Out, "%w) %w (%w)", [T.id, T.titulo, T.status]), write(Out), nl,
    exibeTicketsDisciplinaRecursivo(Ts, SiglaDisciplina)
    ;
    exibeTicketsDisciplinaRecursivo(Ts, SiglaDisciplina).