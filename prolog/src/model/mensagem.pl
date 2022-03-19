:- module('mensagem', [adiciona_mensagem/4]).

adiciona_mensagem(Autor, Conteudo, Horario, NovaMensagem) :-
    writeln("Mensagem adicionada.").
