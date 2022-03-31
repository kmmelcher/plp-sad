:- module('MonitorController', [getMonitor/2, vinculaMonitor/0, ehMonitor/1, desvinculaMonitor/0]).

:- use_module('../util/jsonFunctions.pl', [getObjetoByID/3, addMonitor/4, checaExistencia/2, existeDisciplina/1, removeMonitor/1]).

getMonitor(Id, Monitor):- getObjetoByID("monitores", Id, Monitor).

ehMonitor(Id):- checaExistencia("monitores", Id).

adicionaMonitor(Matricula, Disciplina) :-
    writeln("Horarios:"),
    read(Horarios),
    addMonitor(Matricula, Disciplina, Horarios, ""),
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

excluiMonitor(Matricula) :-
    atom_string(Matricula, MatriculaStr),
    removeMonitor(MatriculaStr),
    writeln("Monitor removido com sucesso").

desvinculaMonitor() :-
    writeln("Matrícula:"),
    read(Matricula),
    (
        checaExistencia("monitores", Matricula) -> excluiMonitor(Matricula);
        writeln('Monitor não cadastrado')
    ).
