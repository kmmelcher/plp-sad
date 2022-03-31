:- module('Menu', [menuPrincipal/0]).
:- use_module('controller/MonitorController.pl', [getMonitor/2, vinculaMonitor/0, desvinculaMonitor/0]).
:- use_module('controller/ChatController.pl', [exibeTicketsDisciplina/1, exibeTicketsAluno/1,getTicketsAluno/2]).
:- use_module('controller/ProfessorController.pl', [getProfessor/2]).
:- use_module('controller/AlunoController.pl', [getAluno/2]).
:- use_module('util/jsonFunctions.pl', [checaExistencia/2, atualizaAtributoAluno/3, atualizaAtributoProfessor/3,atualizaAtributoTicket/3]).
:- use_module('util/EncriptFunctions.pl', [encripta/3]).

menuPrincipal() :- writeln('\n\nBem vindo ao SAD: Sistema de Atendimento ao Discente! :):'),
                 menuLogin().

menuLogin() :-   writeln("Insira seu ID para entrar. Para sair do sistema, digite 'sair': "),
               read(Id),
               decideMenu(Id).

menuAutenticacao(Objeto):-
    write("Insira a sua Senha:"),
    read(Senha),
    encripta(Senha, Objeto.nome, SenhaEncriptada),
    SenhaEncriptada = Objeto.senha.

decideMenu(sair):- halt(0).
decideMenu(Id) :- 
    atom_string(Id, IdString),
    (
      checaExistencia("professores", IdString) -> exibeMenuProfessor(IdString);
      checaExistencia("monitores", IdString) -> exibeMenuAlunoMonitor(IdString);
      checaExistencia("alunos", IdString) -> exibeMenuAluno(IdString);
      write("Insira um valor valido!\n"),
      menuLogin()
    ).

perguntaDisciplina(Disciplinas):-
    length(Disciplinas, Size), Size > 1,
    writeln("Insira a disciplina:"),
    read(SiglaDisciplina), nl,
    atom_string(SiglaDisciplina, SiglaString),
    (
        member(SiglaString, Disciplinas) -> exibeTicketsDisciplina(SiglaString)
        ;
        write("Insira um valor valido!\n")
    )
    ;
    [H|_] = Disciplinas,
    exibeTicketsDisciplina(H).


%----------------------------------------------------- PROFESSOR -----------------------------------------------------%

exibeMenuProfessor(Id):-
    getProfessor(Id, Professor),
    menuAutenticacao(Professor),
    writeln('\n== SAD: MENU PROFESSOR =='),
    swritef(Out, "\nID: %w | Nome: %w | Disciplinas: %w\n", [Professor.id, Professor.nome, Professor.disciplinas]), write(Out),
    writeln('Digite o numero da acao que deseja executar!\n'),
    writeln('1) Exibir tickets\n2) Responder Tickets em andamento\n3) Vincular aluno/monitor\n4) Desvincular aluno/monitor\n5) Alterar senha de acesso\n6) Deslogar\n'),
    read(Opcao),
    decideMenuProfessor(Opcao, Professor),
    exibeMenuProfessor(Id)
    ;
    writeln("Senha incorreta"),
    menuLogin().

decideMenuProfessor(1, Professor) :- perguntaDisciplina(Professor.disciplinas).

decideMenuProfessor(2, _) :-
    vinculaMonitor(),
    exibeMenuProfessor.

decideMenuProfessor(3, Professor):- menuCadastroProfessor(Professor).

decideMenuProfessor(4, Professor):- menuRemocaoProfessor(Professor).

decideMenuProfessor(5, Professor):- 
    menuTrocarSenhaProfessor(Professor),
    exibeMenuProfessor(Professor.id).

decideMenuProfessor(6, _) :- writeln('Deslogando...'), menuPrincipal().

decideMenuProfessor(_, _) :- writeln('Entrada Inválida!').

menuCadastroProfessor(Professor) :- 
    writeln('\nQuem voce deseja vincular?'),
    writeln('1) Vincular aluno\n2) Vincular monitor\n3) Voltar para o menu professor\n'),
    read(Opcao),
    decideMenuCadastro(Opcao, Professor),
    exibeMenuProfessor(Professor.id).

decideMenuCadastro(1, _).

decideMenuCadastro(2, Professor):-
    vinculaMonitor(),
    exibeMenuProfessor(Professor.id).

decideMenuCadastro(3, _).

decideMenuCadastro(_, Professor):- writeln('Entrada Invalida!'), menuCadastroProfessor(Professor).

menuRemocaoProfessor(Professor) :- 
    writeln('\nQuem voce deseja desvincular?'),
    writeln('1) Desvincular aluno\n2) Desvincular monitor\n3) Voltar para o menu professor\n'),
    read(Opcao),
    decideMenuRemocao(Opcao, Professor),
    exibeMenuProfessor(Professor.id).

decideMenuRemocao(1, _).

decideMenuRemocao(2, Professor) :-
    desvinculaMonitor(),
    exibeMenuProfessor(Professor.id).

decideMenuRemocao(3, Professor):- exibeMenuProfessor(Professor.id).

decideMenuRemocao(_, Professor):- writeln('Entrada Invalida!'), menuRemocao(Professor).

menuTrocarSenhaProfessor(Professor):-
    writeln('Digite sua nova senha:'),
    read(Senha),
    encripta(Senha, Professor.nome, SenhaEncriptada),
    atualizaAtributoProfessor(Professor.id, "senha", SenhaEncriptada).

%----------------------------------------------------- MONITOR -----------------------------------------------------%

exibeMenuAlunoMonitor(Id) :- 
    getMonitor(Id, Monitor),
    write('\nFoi identificado que voce eh monitor da disciplina: '), write(Monitor.disciplina),
    write('\nComo deseja entrar no sistema?\n\n1) Entrar como Aluno\n2) Entrar como Monitor de '), write(Monitor.disciplina), nl,
    read(Opcao),
    decideMenuAlunoMonitor(Opcao, Id).
  
decideMenuAlunoMonitor(1, Id) :- exibeMenuAluno(Id).
decideMenuAlunoMonitor(2, Id) :- exibeMenuMonitor(Id).
decideMenuAlunoMonitor(_, _) :- write('\nEntrada Invalida! '), exibeMenuAlunoMonitor().

exibeMenuMonitor(Id) :- 
    getMonitor(Id, Monitor),
    getAluno(Id, Aluno),
    menuAutenticacao(Aluno),
    writeln('\n== SAD: MENU MONITOR =='),
    swritef(Out, '\nID: %w | Nome: %w | Disciplina: %w\n', [Monitor.id, Aluno.nome, Monitor.disciplina]), write(Out),
    writeln('Digite o numero da ação que deseja executar!\n'),
    writeln('1) Exibir todos os tickets\n2) Responder tickets em andamento\n3) Alterar senha de acesso\n4) Deslogar\n'),
    read(Opcao),
    decideMenuMonitor(Opcao, Monitor),
    exibeMenuMonitor(Id)
    ;
    writeln("Senha incorreta"),
    menuLogin().

decideMenuMonitor(1, Monitor) :- exibeTicketsDisciplina(Monitor.disciplina).

decideMenuMonitor(2, _).

decideMenuMonitor(3, _).

decideMenuMonitor(4, _) :- write('\nDeslogando...'), menuPrincipal().

decideMenuMonitor(_) :- write('\nEntrada Invalida! ').

%----------------------------------------------------- ALUNO -----------------------------------------------------%

exibeMenuAluno(Id):-
    getAluno(Id, Aluno),
    menuAutenticacao(Aluno),
    writeln('\n== SAD: MENU ALUNO =='),
    swritef(Out, '\nID: %w | Nome: %w | Disciplinas: %w\n', [Aluno.id, Aluno.nome, Aluno.disciplinas]), write(Out),
    writeln('Digite o numero da acao que deseja executar!\n'),
    writeln('1) Ler tickets de uma disciplina\n2) Ler meus tickets\n3) Criar Ticket\n4) Mandar mensagem em um ticket meu\n5) Marcar ticket como resolvido\n6) Excluir ticket\n7) Trocar senha de acesso\n8) Deslogar\n'),
    read(Opcao),
    decideMenuAluno(Opcao, Aluno),
    exibeMenuAluno(Id)
    ;
    writeln("Senha incorreta"),
    menuLogin().

decideMenuAluno(1, Aluno):- perguntaDisciplina(Aluno.disciplinas).

decideMenuAluno(2, Aluno):- exibeTicketsAluno(Aluno.id).

decideMenuAluno(3, _).

decideMenuAluno(4, _).

decideMenuAluno(5, Aluno) :-
    getTicketsAluno(Aluno.id,TicketsAluno),
    write(TicketsAluno),
    exibeTicketsAluno(Aluno.id),
    writeln('\nInsira o id do ticket que deseja marcar como concluído:'),
    read(Opcao),
    atualizaAtributoTicket(Opcao,"status","Resolvido").

decideMenuAluno(6, _).

decideMenuAluno(7, Aluno):- 
    menuTrocarSenhaAluno(Aluno),
    exibeMenuAluno(Aluno.id).

decideMenuAluno(8, _) :- write('\nDeslogando...'), menuPrincipal().

decideMenuAluno(_, _) :- write('\nEntrada Invalida! ').

menuTrocarSenhaAluno(Aluno):-
    writeln('Digite sua nova senha:'),
    read(Senha),
    encripta(Senha, Aluno.nome, SenhaEncriptada),
    atualizaAtributoAluno(Aluno.id, "senha", SenhaEncriptada).