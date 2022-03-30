:- module('MonitorController', [getMonitor/2, vinculaMonitor/0, ehMonitor/1]).

:- use_module('../util/jsonFunctions.pl', [getObjetoByID/3, checaExistencia/2]).

getMonitor(Id, Monitor):- getObjetoByID("monitores", Id, Monitor).

ehMonitor(Id):- checaExistencia("monitores", Id).

vinculaMonitor() :-
    writeln("Disciplina:"),
    read(Disciplina), % Checar no menu se o professor possui 1 ou 2 disciplinas para automatizar esse processo
    writeln("Matrícula:"),
    read(Matricula),
    (checaExistencia("alunos", Matricula) -> writeln('Cadastra monitor');
    writeln('Aluno não cadastrado')).
