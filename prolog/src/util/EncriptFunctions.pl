:- module('EncriptFunctions', [encripta/3]).

%Encripta uma mensagem(string já no formato de lista)
encriptaMensagem([],_,'').
encriptaMensagem([Mensagem|TailMensagem], [Chave|TailChave], Saida):-
    encriptaCaracter(Mensagem, Chave, SaidaCaracter),
    encriptaMensagem(TailMensagem, TailChave, Saida1),
    string_concat(Saida1, SaidaCaracter, Saida).

%Encripta um unico caractere
encriptaCaracter(Mensagem, Chave, Saida):-
    char_code(Chave, Code),
    char_code(Mensagem, CodeM),
    N is (CodeM + Code)*Code,
    number_string(N, Saida).

%Deixa uma chave propicia a ser utilizada
arrumaChave(Mensagem, Chave, Saida):-
    length(Mensagem, M),
    length(Chave, C),
    (C < M -> append(Chave, Chave, Saida1), arrumaChave(Mensagem, Saida1, Saida); append(Chave, [] , Saida)).

%Encripta uma mensagem de uma string qualquer e uma chave qualquer
encripta(Mensagem, Chave, Saida):-
    atom_chars(Mensagem, Mensagem_list),
    atom_chars(Chave, Chave_list),
    arrumaChave(Mensagem_list, Chave_list, Chave_arrumada),
    encriptaMensagem(Mensagem_list, Chave_arrumada, Saida).
    