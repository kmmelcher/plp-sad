:- use_module('controller/MonitorController.pl', [vinculaMonitor/0, getMonitor/2]).
:- use_module('controller/ChatController.pl', [exibeTicketsDisciplina/1]).
:- use_module('controller/ProfessorController.pl', [getProfessor/2]).
:- use_module('controller/AlunoController.pl', [getAluno/2]).
:- use_module('util/jsonFunctions', [checaExistencia/2, getObjetoByID/3]).

menuPrincipal() :- writeln('Bem vindo ao SAD: Sistema de Atendimento ao Discente! :):'),
                 menuLogin().

menuLogin() :-   write("\nInsira seu ID para entrar. Para sair do sistema, digite 'sair': "),
               read(Id),
               decideMenu(Id).

decideMenu(sair):- halt(0).
decideMenu(Id) :- 
    (
      jsonFunctions:checaExistencia("professores", Id) -> exibeMenuProfessor(Id);
      jsonFunctions:checaExistencia("monitores", Id) -> exibeMenuAlunoMonitor(Id);
      jsonFunctions:checaExistencia("alunos", Id) -> exibeMenuAluno(Id);
      write("Insira um valor válido!\n"),
      menuLogin()
    ).

menuTrocarSenha():- 
    writeln('Digite sua nova senha:').

%----------------------------------------------------- PROFESSOR -----------------------------------------------------%

exibeMenuProfessor(Id) :- 
    getProfessor(Id, Professor),

    writeln('== SAD: MENU PROFESSOR =='),
    swritef(Out, "\nID: %w | Nome: %w | Disciplinas: %w", [Professor.id, Professor.nome, Professor.disciplinas]), write(Out),
    writeln('Digite o número da ação que deseja executar!'),
    writeln('1) Exibir tickets\n2) Responder Tickets em andamento\n3) Vincular aluno/monitor\n4) Desvincular aluno/monitor\n5) Alterar senha de acesso\n6) Deslogar\n'),
    read(Opcao),
    decideMenuProfessor(Opcao, Professor),
    exibeMenuProfessor(Id).

decideMenuProfessor(1, Professor) :- 
    length(Professor.disciplinas, Size), Size > 1,
    writeln("Insira a disciplina:"),
    read(SiglaDisciplina),
    atom_string(SiglaDisciplina, SiglaString),
    (
        member(SiglaString, Professor.disciplinas) -> exibeTicketsDisciplina(SiglaString)
        ;
        write("Insira um valor válido!\n")
    )
    ;
    [H|_] = Professor.disciplinas,
    exibeTicketsDisciplina(H).

decideMenuProfessor(2, _).

decideMenuProfessor(3, Professor):- menuCadastroProfessor(Professor).

decideMenuProfessor(4, Professor):- menuRemocaoProfessor(Professor).

decideMenuProfessor(5, _):- menuTrocarSenha().

decideMenuProfessor(6, _) :- writeln('Deslogando...'), menuPrincipal().

decideMenuProfessor(_, _) :- writeln('Entrada Inválida!').

menuCadastroProfessor(Professor) :- 
    writeln('Quem você deseja vincular?'),
    writeln('1) Vincular aluno\n2) Vincular monitor\n3) Voltar para o menu professor\n'),
    read(Opcao),
    decideMenuCadastro(Opcao, Professor),
    exibeMenuProfessor(Professor.id).

decideMenuCadastro(1, _).

decideMenuCadastro(2, _):- vinculaMonitor().

decideMenuCadastro(3, _).

decideMenuCadastro(_, Professor):- writeln('Entrada Inválida!'), menuCadastroProfessor(Professor).

menuRemocaoProfessor(Professor) :- 
    writeln('Quem você deseja desvincular?'),
    writeln('1) Desvincular aluno\n2) Desvincular monitor\n3) Voltar para o menu professor\n'),
    read(Opcao),
    decideMenuRemocao(Opcao, Professor),
    exibeMenuProfessor(Professor.id).

decideMenuRemocao(1, _).

decideMenuRemocao(2, _).

decideMenuRemocao(3, Professor):- exibeMenuProfessor(Professor.id).

decideMenuRemocao(_, Professor):- writeln('Entrada Inválida!'), menuRemocao(Professor).

%----------------------------------------------------- MONITOR -----------------------------------------------------%

exibeMenuAlunoMonitor(Id) :- 
    getMonitor(Id, Monitor),

    writeln('Foi identificado que você é monitor da disciplina: '), write(Monitor.disciplina),
    writeln('Como deseja entrar no sistema?\n\n1) Entrar como Aluno\n2) Entrar como Monitor de '), write(Monitor.disciplina),
    read(Opcao),
    decideMenuAlunoMonitor(Opcao, Id).

decideMenuAlunoMonitor(1, Id) :- exibeMenuAluno(Id).
decideMenuAlunoMonitor(2, Id) :- exibeMenuMonitor(Id).
decideMenuAlunoMonitor(_) :- write('\nEntrada Inválida! '), exibeMenuAlunoMonitor().

exibeMenuMonitor(Id) :- 
    getMonitor(Id, Monitor),

    write('\n== SAD: MENU MONITOR =='),
    swritef(Out, '\nID: %w| Nome: %w| Disciplina: %w', [Monitor.id, Monitor.nome, Monitor.disciplina]), write(Out),
    write('\nDigite o número da ação que deseja executar!'),
    write('\n1) Exibir todos os tickets\n2) Responder tickets em andamento\n3) Alterar senha de acesso\n4) Deslogar\n'),
    read(Opcao),
    decideMenuMonitor(Opcao, Monitor),
    exibeMenuMonitor(Id).

decideMenuMonitor(1, Monitor) :- exibeTicketsDisciplina(Monitor.disciplina).

decideMenuMonitor(2, _).

decideMenuMonitor(3, _):- menuTrocarSenha().

decideMenuMonitor(4, _) :- write('\nDeslogando...'), menuPrincipal().

decideMenuMonitor(_) :- write('\nEntrada Inválida! ').

%----------------------------------------------------- ALUNO -----------------------------------------------------%

exibeMenuAluno(Id):-
    getAluno(Id, Aluno),

    write('\n== SAD: MENU ALUNO =='),
    swritef(Out, '\nID: %w| Nome: %w| Disciplinas: %w', [Aluno.id, Aluno.nome, Aluno.disciplinas]), write(Out),
    write('\nDigite o número da ação que deseja executar!'),
    write('\n1) Ler tickets de uma disciplina\n2) Ler meus tickets\n3) Criar Ticket\n4) Mandar mensagem em um ticket meu\n5) Marcar ticket como resolvido\n6) Excluir ticket\n7) Trocar senha de acesso\n8) Deslogar\n'),
    read(Opcao),
    decideMenuAluno(Opcao, Aluno),
    exibeMenuAluno(Id).

decideMenuAluno(1, _).

decideMenuAluno(2, _).

decideMenuAluno(3, _).

decideMenuAluno(4, _).

decideMenuAluno(5, _).

decideMenuAluno(6, _).

decideMenuAluno(7, _) :- menuTrocarSenha().

decideMenuAluno(8, _) :- write('\nDeslogando...'), menuPrincipal().

decideMenuAluno(_, _) :- write('\nEntrada Inválida! ').
