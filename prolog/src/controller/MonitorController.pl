:- module('MonitorController', [getMonitor/2, vinculaMonitor/0]).

:- include("../util/jsonFunctions.pl").
:- use_module('AlunoController.pl', [getAluno/2]).

getMonitor(Id, Monitor):-
    checaExistencia("monitores", Id),
    getObjetoByID("monitores", Id, Monitor).

adicionaMonitor(Matricula, Disciplina) :-
    writeln("Horarios:"),
    read(Horarios),
    addMonitor(Matricula, Disciplina, Horarios),
    writeln("Monitor cadastrado com sucesso.").

vinculaMonitor() :-
    writeln("Disciplina:"),
    read(Disciplina),
    (
        existeDisciplina(Disciplina) ->
            writeln("Matrícula:"),
            read(Matricula),
            (
                checaExistencia("alunos", Matricula) -> adicionaMonitor(Matricula, Disciplina);
                writeln('Aluno não cadastrado')
            );
        writeln('Disciplina não cadastrada')
    ).
