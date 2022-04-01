:- module(monitorController, 
    [
        getMonitor/2,
        vinculaMonitor/1,
        ehMonitor/1,
        desvinculaMonitor/1,
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
:- use_module('../controller/AlunoController.pl', [ehAluno/1, getAluno/2]).

getMonitor(Id, Monitor):- getObjetoByID("monitores", Id, Monitor).

ehMonitor(Id):- checaExistencia("monitores", Id).

adicionaMonitor(Matricula, Disciplina) :-
    writeln("Horarios de atendimento do monitor:"),
    read(Horarios),
    addMonitor(Matricula, Disciplina, Horarios),
    writeln("Monitor cadastrado com sucesso.").

vinculaMonitor(Disciplina) :-
    writeln("Matricula:"),
    read(MatriculaAtom), atom_string(MatriculaAtom, Matricula),
    (
        ehAluno(Matricula) ->
            getAluno(Matricula, Aluno),
            (
                member(Disciplina, Aluno.disciplinas) -> writeln("Este aluno esta cursando sua disciplina!\n");
                ehMonitor(Matricula) -> writeln("Este monitor ja esta vinculado a uma disciplina\n");
                adicionaMonitor(Matricula, Disciplina)
            );
        writeln('Aluno nao cadastrado')
    ).

excluiMonitor(Matricula) :-
    atom_string(Matricula, MatriculaStr),
    removeMonitor(MatriculaStr),
    writeln("Monitor removido com sucesso").

desvinculaMonitor(Disciplina) :-
    writeln("Matricula:"),
    read(MatriculaAtom), atom_string(MatriculaAtom, Matricula),
    (
        ehAluno(Matricula) -> 
            (
                ehMonitor(Matricula) -> 
                    getMonitor(Matricula, Monitor),
                    (
                        Monitor.disciplina = Disciplina -> excluiMonitor(Matricula);
                        writeln("Monitor nao pertence a esta disciplina")
                    );
                writeln("Aluno nao eh monitor")
            );

        writeln("Matricula nao encontrada")
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
