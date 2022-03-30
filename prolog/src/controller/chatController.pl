:- include("../util/jsonFunctions.pl").

exibeTicketsDisciplina(SiglaDisciplina):-
    readJSON("tickets", Tickets),
    exibeTicketsDisciplinaRecursivo(Tickets, SiglaDisciplina).

exibeTicketsDisciplinaRecursivo([], _):- write('chegou 1').

exibeTicketsDisciplinaRecursivo([T|Ts], SiglaDisciplina):-
    (
        T.disciplina = SiglaDisciplina -> writeln(T.id), nl;
        exibeTicketsDisciplinaRecursivo(Ts, SiglaDisciplina)
    ), 
    exibeTicketsDisciplinaRecursivo(Ts, SiglaDisciplina).