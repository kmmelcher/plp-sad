:- module(monitorController, 
    [
        getMonitor/2,
        vinculaMonitor/0,
        ehMonitor/1,
        desvinculaMonitor/0,
        listarMonitoresByDisciplina/1
    ]).

:- use_module('../util/jsonFunctions', 
    [
        getObjetoByID/3,
        addMonitor/3,
        checaExistencia/2,
        existeDisciplina/1,
        removeMonitor/1,
        readJSON/2,
        showMonitoresAux/1
    ]).

getMonitor(Id, Monitor):- getObjetoByID("monitores", Id, Monitor).

ehMonitor(Id):- checaExistencia("monitores", Id).

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

getMonitoresByDisciplinaRecursivo([], _, []).
getMonitoresByDisciplinaRecursivo([H|T], Sigla, Monitores) :-
    H.disciplina = Sigla,
    getMonitoresByDisciplinaRecursivo(T, Sigla, MonitoresAux),
    append(MonitoresAux, [H], Monitores)
    ;
    getMonitoresByDisciplinaRecursivo(T, Sigla, Monitores).

getMonitoresByDisciplina(Sigla, Monitores):-
    readJSON("monitores", File),
    getMonitoresByDisciplinaRecursivo(File, Sigla, Monitores).

listarMonitoresByDisciplina(Disciplina) :-
    getMonitoresByDisciplina(Disciplina, Monitores),
    (
        Monitores = [] -> writeln("Não há monitores nessa disciplina"); 
        showMonitoresAux(Monitores)
    ).
