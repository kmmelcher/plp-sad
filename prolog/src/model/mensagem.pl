:- module('mensagem', [adiciona_mensagem/5]).

adiciona_mensagem(Id, Autor, Conteudo, Horario, NovaMensagem) :-
    writeln("Mensagem adicionada.").
