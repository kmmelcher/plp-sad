:- include("../controller/AlunoController.pl").

getTicketsAluno(Matricula, Saida):-
    readJSON("tickets", TodosTickets),
    ticketsDoAluno(TodosTickets, Matricula, Saida).
    
ticketsDoAluno([],_, []).
ticketsDoAluno([H|T],Matricula, Tickets):-
    H.autor = Matricula,
    %write(H.autor),
    ticketsDoAluno(T, Matricula, TicketsS),
    append(TicketsS, [H], Tickets)
    ;
    ticketsDoAluno(T, Matricula, Tickets).

exibeTicketsAluno(Matricula):-
    getTicketsAluno(Matricula, Tickets),
    exibirTickets(Tickets).

exibirTickets([]).
exibirTickets([H|T]):-
    swritef(Out, "%w) %w (%w)", [H.id, H.titulo, H.status]), write(Out), nl,
    exibirTickets(T).