:- module('MonitorController', [getMonitor/2, vinculaMonitor/0]).

:- use_module('../util/jsonFunctions.pl', [checaExistencia/2, getObjetoByID/3]).

getMonitor(Id, Monitor):-
    checaExistencia("monitores", Id),
    getObjetoByID("monitores", Id, Monitor).

vinculaMonitor() :-
    writeln("Disciplina:"),
    read(Disciplina),
    writeln("Matrícula:"),
    read(Matricula),
    (checaExistencia("alunos", Matricula) -> writeln('Cadastra monitor');
    writeln('Aluno não cadastrado')),
    halt.
