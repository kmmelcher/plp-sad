:- module('MonitorController', [getMonitor/2, vinculaMonitor/0]).

:- use_module('../util/jsonFunctions.pl', [getObjetoByID/3]).

getMonitor(Id, Monitor):-
    getObjetoByID("monitores", Id, Monitor).

vinculaMonitor() :-
    writeln("Disciplina:"),
    read(Disciplina), % Checar no menu se o professor possui 1 ou 2 disciplinas para automatizar esse processo
    writeln("Matrícula:"),
    read(Matricula),
    (checaExistencia("alunos", Matricula) -> writeln('Cadastra monitor');
    writeln('Aluno não cadastrado')).
