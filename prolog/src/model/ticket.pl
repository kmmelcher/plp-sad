:- module('ticket', [adiciona_ticket/6]).

adiciona_ticket(Autor, Titulo, Mensagens, Status, Disciplina, NovoTicket) :-
    writeln("Ticket adicionado.").
