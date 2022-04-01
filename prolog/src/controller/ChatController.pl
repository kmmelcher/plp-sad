:- module('ChatController', [exibeTicketsDisciplina/1, responderTicket/2, exibeTicketsAluno/2, getTicketsAluno/2,marcarTicketAlunoComoResolvido/1,adicionarMensagemTicketAluno/1, adicionaTicket/2, excluirTicket/1]).
:- use_module('../util/jsonFunctions', [readJSON/2, getObjetoByID/3, atualizaAtributoTicket/3, addMensagem/4, addTicket/6, removeMensagem/1, removeTicket/1]).
:- use_module('AlunoController.pl', [getAluno/2, ehAluno/1]).
:- use_module('MonitorController.pl', [getMonitor/2, ehMonitor/1]).
:- use_module('ProfessorController.pl', [ehProfessor/1, getProfessor/2]).
%----------------------------------------------------- FUNÇÕES DE GET -----------------------------------------------------%

getTicket(Id, Ticket):-
    getObjetoByID("tickets", Id, TicketJson),
    split_string(TicketJson.mensagens, ",", "", MensagensFormated),
    put_dict([mensagens=MensagensFormated], TicketJson, Ticket).

getMensagem(Id, Mensagem):- getObjetoByID("mensagens", Id, Mensagem).

getTicketsAluno(Matricula, Saida):-
    readJSON("tickets", TodosTickets),
    getTicketsDoAlunoRecursivo(TodosTickets, Matricula, Saida).

getTicketsDoAlunoRecursivo([],_, []).
getTicketsDoAlunoRecursivo([H|T],Matricula, Tickets):-
    H.autor = Matricula,
    getTicketsDoAlunoRecursivo(T, Matricula, TicketsS),
    append(TicketsS, [H], Tickets)
    ;
    getTicketsDoAlunoRecursivo(T, Matricula, Tickets).

getTicketsDisciplina(SiglaDisciplina, TicketsDisciplina):-
    readJSON("tickets", Tickets),
    getTicketsDisciplinaRecursivo(Tickets, SiglaDisciplina, [], TicketsDisciplina).

getTicketsDisciplinaRecursivo([], _, TicketsAux, TicketsDisciplina):- TicketsDisciplina = TicketsAux.
getTicketsDisciplinaRecursivo([T|Ts], SiglaDisciplina, TicketsAux, TicketsDisciplina):-
    T.disciplina = SiglaDisciplina,
    append(TicketsAux, [T], NovosTicketsDisciplina),
    getTicketsDisciplinaRecursivo(Ts, SiglaDisciplina, NovosTicketsDisciplina, TicketsDisciplina)
    ;
    getTicketsDisciplinaRecursivo(Ts, SiglaDisciplina, TicketsAux, TicketsDisciplina).

getTicketsEmAndamento(Disciplina, TicketsEmAndamento):-
    getTicketsDisciplina(Disciplina, Tickets),
    getTicketsEmAndamentoRecursivo(Tickets, [], TicketsEmAndamento).

getTicketsEmAndamentoRecursivo([], TicketsAux, TicketsDisciplina):- TicketsDisciplina = TicketsAux.
getTicketsEmAndamentoRecursivo([T|Ts], TicketsAux, TicketsEmAndamento):-
    T.status = "Em andamento",
    append(TicketsAux, [T], NovosTickets),
    getTicketsEmAndamentoRecursivo(Ts, NovosTickets, TicketsEmAndamento);
    getTicketsEmAndamentoRecursivo(Ts, TicketsAux, TicketsEmAndamento).

%----------------------------------------------------- FUNÇÕES DE EXIBIÇÃO -----------------------------------------------------%
exibirTickets([]).
exibirTickets([H|T]):-
    swritef(Out, "%w) %w (%w)", [H.id, H.titulo, H.status]), write(Out), nl,
    exibirTickets(T).

exibeTicketsAluno(Matricula, Ticket):-
    getTicketsAluno(Matricula, Tickets),
    (Tickets = [] -> writeln("Voce ainda nao criou nenhum ticket."); 
        writeln("Estes sao os tickets criados por voce\n"),
        exibirTickets(Tickets), 
        writeln("\nInsira o id do ticket que deseja visualizar mensagens:\n"),
        read(OpcaoAtom), atom_string(OpcaoAtom, Opcao),
        (
            checaIdValidoEmTickets(Opcao, Tickets) -> 
                getTicket(Opcao, Ticket), 
                (Ticket.mensagens = [""] -> writeln("Ainda nao ha mensagens nesse ticket.");
                exibeMensagensTicket(Ticket.mensagens, Ticket.disciplina));
            msgInputInvalido()
        ) 
    ).

exibeTicketsDisciplina(SiglaDisciplina):-
    getTicketsDisciplina(SiglaDisciplina, Tickets),
    (
        Tickets = [] -> writeln("Ainda nao ha tickets para esta disciplina.")
        ;
        swritef(Out, "Estes sao os tickets da disciplina: %w\n", [SiglaDisciplina]), write(Out),
        exibirTickets(Tickets),
        writeln("\n Qual o numero do ticket que deseja visualizar mensagens?"), 
        read(Opcao),
        atom_string(Opcao, OpcaoStr),
        (
            checaIdValidoEmTickets(OpcaoStr, Tickets) -> 
                getTicket(OpcaoStr, Ticket),
                (
                    Ticket.mensagens = [""] -> writeln("Ainda nao ha mensagens neste ticket");
                    exibeMensagensTicket(Ticket.mensagens, Ticket.disciplina)
                ); 
            msgInputInvalido()
        )
    ).

checaIdValidoEmTickets(_, []):- false.
checaIdValidoEmTickets(Opcao, [T|Ts]):- Opcao = T.id ; checaIdValidoEmTickets(Opcao, Ts).

exibeMensagensTicket([], _).
exibeMensagensTicket([Im|Ims], Disciplina):-
    getMensagem(Im, Mensagem),
    checaEntidadeParaMensagem(Mensagem.autor, Disciplina, Autor, Entidade),
    swritef(Out, "[%w] (%w) %w - %w\n", [Mensagem.horario, Autor, Entidade.nome, Mensagem.conteudo]), write(Out),
    exibeMensagensTicket(Ims, Disciplina).


responderTicket(Entidade, Disciplina):-
    getTicketsEmAndamento(Disciplina, Tickets),
    (
      Tickets = [] -> writeln("Ainda nao ha tickets em andamento para serem respondidos nesta disciplina.\n") ;
      swritef(Out, "Estes sao os tickets em andamento da disciplina: %w\n\n", [Disciplina]), write(Out),
        exibirTickets(Tickets),
        writeln("Qual ticket voce deseja responder? "), read(Opcao), atom_string(Opcao, OpcaoStr),
        (
            checaIdValidoEmTickets(OpcaoStr, Tickets) -> getTicket(OpcaoStr, Ticket), adicionaMensagem(Entidade, Ticket);
            msgInputInvalido()
        )
    ).

adicionaMensagem(Entidade, Ticket):-
    checaEntidadeParaMensagem(Entidade.id, Ticket.disciplina, _, _),
    writeln("Insira a mensagem entre aspas simples: "), read(Conteudo),
    get_time(T), format_time(string(Horario), "%c", T),
    addMensagem(Entidade.id, Conteudo, Horario, IdMensagem),
    atualizaAtributoTicket(Ticket.id, "mensagens", IdMensagem),   
    writeln("Mensagem adicionada com sucesso.\n").

checaEntidadeParaMensagem(Id, Disciplina, Autor, Entidade):-
    ehMonitor(Id), getMonitor(Id, Monitor), Monitor.disciplina = Disciplina -> Autor = "MONITOR", getAluno(Id, Entidade);
    ehProfessor(Id) -> Autor = "PROFESSOR", getProfessor(Id, Entidade);
    Autor = "ALUNO", getAluno(Id, Entidade).

msgInputInvalido():- writeln("Insira um valor valido\n").

marcarTicketAlunoComoResolvido(Aluno) :-
    getTicketsAluno(Aluno.id,TicketsAluno),
    exibeTicketsAluno(Aluno.id, _),
    writeln('\nInsira o id do ticket que deseja marcar como concluído:'),
    read(Opcao), atom_string(Opcao,OpcaoStr),
    (verificarIdTicketAoResolver(TicketsAluno,Aluno.id) -> atualizaAtributoTicket(OpcaoStr,"status","Resolvido"), writeln("Status de ticket atualizado com sucesso") ; writeln('Id invalido !')).

verificarIdTicketAoResolver([H|T],AutorId) :-
    (H.autor = AutorId ->
        H.status = "Em andamento";
        verificarIdTicketAoResolver(T,AutorId)).

verificarIdTicketAoMandarMsg(IdTicket,Aluno) :-
    getTicket(IdTicket,Ticket),
    Ticket.autor = Aluno.id,
    Ticket.status = "Em andamento".

adicionarMensagemTicketAluno(Aluno) :-
    exibeTicketsAluno(Aluno.id, Ticket),
    adicionaMensagem(Aluno, Ticket).
    
adicionaTicket(Aluno, Disciplina):-
    writeln("Insira o titulo do seu ticket (entre aspas simples): "),
    read(Titulo),
    addTicket("", Titulo, Aluno.id, [""], "Em andamento", Disciplina),
    writeln("Ticket adicionado com sucesso.").

removeMensagens(IdTicket):-
    getTicket(IdTicket, Ticket),
    removeMensagensRecursivo(Ticket.mensagens).

removeMensagensRecursivo([]).
removeMensagensRecursivo([H|T]):- removeMensagem(H), removeMensagensRecursivo(T).

excluirTicket(IdAluno):-
    getTicketsAluno(IdAluno, Tickets),
    (
        Tickets = [] -> writeln("Nao existe nenhum ticket cadastrado por voce");
        writeln("\nEstes sao os tickets criados por voce:"),
        exibirTickets(Tickets),
        writeln("\nQual ticket voce deseja exlcuir?\n"),
        read(Opcao), atom_string(Opcao, OpcaoStr),
        (checaIdValidoEmTickets(OpcaoStr, Tickets) -> 
            removeMensagens(OpcaoStr),
            removeTicket(OpcaoStr), 
            msgInputInvalido(),
            writeln("Ticket deletado com sucesso\n");
            msgInputInvalido()
        )
    ).
