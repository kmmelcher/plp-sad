:- module('ticket', [adiciona_ticket/7]).

adiciona_ticket(Id, Autor, Titulo, Mensagens, Status, Disciplina, NovoTicket) :-
    writeln("Ticket adicionado.").
