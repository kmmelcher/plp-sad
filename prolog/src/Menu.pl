
:- use_module('model/aluno.pl', [cadastra_aluno/5] ). % Exemplo de importação

menuPrincipal :- write('\n\nBem vindo ao SAD: Sistema de Atendimento ao Discente! :):'),
                 menuLogin.

menuLogin :-   write('\nEscolha o menu que vc deseja acessar: \n1) Professor \n2) Monitor \n3) Aluno \n4) Sair\n'),
               read(Opcao),
               decideMenu(Opcao).

decideMenu(1) :- exibeMenuProfessor, !.
decideMenu(2) :- exibeMenuAlunoMonitor, !.
decideMenu(3) :- exibeMenuAluno, !.
decideMenu(4) :- halt , !.
decideMenu(_) :- write('\nEntrada Inválida! '),
                 menuLogin.

exibeMenuProfessor :- write('\n== SAD: MENU PROFESSOR =='),
                      write('\nID: | Nome: | Disciplinas: '),
                      write('\nDigite o número da ação que deseja executar!'),
                      write('\n1) Exibir tickets\n2) Responder Tickets em andamento\n3) Vincular aluno/monitor\n4) Desvincular aluno/monitor\n5) Alterar senha de acesso\n6) Cadastrar\n7) Deslogar\n'),
                      read(Opcao),
                      decideMenuProfessor(Opcao).

decideMenuProfessor(1) :- exibeMenuProfessor, !.

decideMenuProfessor(2) :- exibeMenuProfessor, !.

decideMenuProfessor(3) :- menuCadastro, !.

decideMenuProfessor(4) :- menuRemocao, !.

decideMenuProfessor(5) :- menuTrocarSenhaProfessor, !.

decideMenuProfessor(6) :-
    aluno:cadastra_aluno(1, 'Vinicius', 'aluno123', ['OAC'], aluno), !. % Exemplo de uso

decideMenuProfessor(7) :- write('\nDeslogando...'),
                          menuPrincipal, !.
decideMenuProfessor(_) :- write('\nEntrada Inválida! '),
                          exibeMenuProfessor.

menuCadastro :- write('\nQuem você deseja vincular?'),
                write('\n1) Vincular aluno\n2) Vincular monitor\n3) Voltar para o menu professor\n'),
                read(Opcao),
                decideMenuCadastro(Opcao).

decideMenuCadastro(1) :- exibeMenuProfessor, !.

decideMenuCadastro(2) :- exibeMenuProfessor, !.

decideMenuCadastro(3) :- exibeMenuProfessor, !.

decideMenuCadastro(_) :- write('\nEntrada Inválida! '),
                         menuCadastro.

menuRemocao :- write('\nQuem você deseja desvincular?'),
               write('\n1) Desvincular aluno\n2) Desvincular monitor\n3) Voltar para o menu professor\n'),
               read(Opcao),
               decideMenuRemocao(Opcao).

decideMenuRemocao(1) :- exibeMenuProfessor, !.

decideMenuRemocao(2) :- exibeMenuProfessor, !.

decideMenuRemocao(3) :- exibeMenuProfessor, !.

decideMenuRemocao(_) :- write('\nEntrada Inválida! '),
                        menuRemocao.

menuTrocarSenhaProfessor :- write('\nDigite sua nova senha: \n'),
                            read(Senha),
                            exibeMenuProfessor.

exibeMenuAlunoMonitor :-  write('\nFoi identificado que você é monitor da disciplina: '),
                          write('\nComo deseja entrar no sistema?\n\n1) Entrar como Aluno\n2) Entrar como Monitor de \n'),
                          read(Opcao),
                          decideMenuAlunoMonitor(Opcao).

decideMenuAlunoMonitor(1) :- exibeMenuAluno, !.
decideMenuAlunoMonitor(2) :- exibeMenuMonitor, !.
decideMenuAlunoMonitor(_) :- write('\nEntrada Inválida! '),
                             exibeMenuAlunoMonitor.

exibeMenuMonitor :- write('\n== SAD: MENU MONITOR =='),
                    write('\nID: | Nome: | Disciplina: '),
                    write('\nDigite o número da ação que deseja executar!'),
                    write('\n1) Exibir todos os tickets\n2) Responder tickets em andamento\n3) Deslogar\n'),
                    read(Opcao),
                    decideMenuMonitor(Opcao).

decideMenuMonitor(1) :- exibeMenuMonitor,!.

decideMenuMonitor(2) :- exibeMenuMonitor,!.

decideMenuMonitor(3) :- write('\nDeslogando...'),
                        menuPrincipal,!.
decideMenuMonitor(_) :- write('\nEntrada Inválida! '),
                        exibeMenuMonitor.

exibeMenuAluno:- write('\n== SAD: MENU ALUNO =='),
                 write('\nID: | Nome: | Disciplinas: '),
                 write('\nDigite o número da ação que deseja executar!'),
                 write('\n1) Ler tickets de uma disciplina\n2) Ler meus tickets\n3) Criar Ticket\n4) Mandar mensagem em um ticket meu\n5) Marcar ticket como resolvido\n6) Excluir ticket\n7) Trocar senha de acesso\n8) Deslogar\n'),
                 read(Opcao),
                 decideMenuAluno(Opcao).

decideMenuAluno(1) :- exibeMenuAluno,!.

decideMenuAluno(2) :- exibeMenuAluno,!.

decideMenuAluno(3) :- exibeMenuAluno,!.

decideMenuAluno(4) :- exibeMenuAluno,!.

decideMenuAluno(5) :- exibeMenuAluno,!.

decideMenuAluno(6) :- exibeMenuAluno,!.

decideMenuAluno(7) :- menuTrocarSenhaAluno,!.

decideMenuAluno(8) :- write('\nDeslogando...'),
                      menuPrincipal,!.
decideMenuAluno(_) :- write('\nEntrada Inválida! '),
                      exibeMenuAluno.

menuTrocarSenhaAluno :- write('\nDigite sua nova senha: \n'),
                        read(Senha),
                        exibeMenuAluno.
