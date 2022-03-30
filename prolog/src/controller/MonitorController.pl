:- module('MonitorController', [getMonitor/2, vinculaMonitor/0]).

:- use_module('AlunoController.pl', [getAluno/2]).
:- use_module('../util/jsonFunctions.pl', [getObjetoByID/3, addMonitor/3, checaExistencia/2, existeDisciplina/1]).

getMonitor(Id, Monitor):-
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
