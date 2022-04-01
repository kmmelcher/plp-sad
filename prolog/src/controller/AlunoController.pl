:- module('AlunoController', [getAluno/2, ehAluno/1, vinculaAlunoDisciplina/1, removeAluno/1, desvinculaAlunoDisciplina/1]).
:- use_module('../util/jsonFunctions.pl', [getObjetoByID/3, atualizaAtributoAluno/3, checaExistencia/2, addAluno/4, removeAluno/1]).
:- use_module('../controller/MonitorController.pl', [ehMonitor/1]).

getAluno(Id, Aluno):-
    getObjetoByID("alunos", Id, AlunoJson),
    split_string(AlunoJson.disciplinas, ",", "", DisciplinasFormated),
    put_dict([disciplinas=DisciplinasFormated], AlunoJson, Aluno).

ehAluno(Id):- checaExistencia("alunos", Id).

vinculaAlunoDisciplina(Disciplina) :-
    writeln("Digite a matrícula do Aluno (caso o aluno ainda não exista, digite 0"),
    read(Id),
    atom_string(Disciplina, DisciplinaStr),
    (ehAluno(Id) -> getAluno(Id, Aluno),
        (ehMonitor(Id), member(DisciplinaStr, Aluno.disciplinas) -> 
            writeln("Este aluno é monitor de sua disciplina!");
                append(Aluno.disciplinas, [DisciplinaStr], NovasDisciplinas),
                atualizaAtributoAluno(Aluno.id, "disciplinas", NovasDisciplinas)
        );
        writeln("Este aluno não está cadastrado no SAD."),
        cadastraAluno("", [Disciplina])
    ).
    
cadastraAluno(Nome, Disciplina):-
    (Nome = "" -> 
        writeln("Digite o nome do ingressante (entre aspas duplas): "),
        read(NomeAluno);
            NomeAluno = Nome),
    
    (Disciplina = "" ->
            writeln("Digite a disciplina do ingressante: "),
            read(DisciplinaAluno);
            DisciplinaAluno = Disciplina
    ),
    writeln("Digite a matrícula do ingressante: "),
    read(Matricula),
    addAluno(Matricula, NomeAluno, DisciplinaAluno, ""), !. 

removerAluno(Id):-
    removeAluno(Id),
    writeln("Aluno removido com sucesso!").

desvinculaAlunoDisciplina(Disciplina) :-
    writeln("Matrícula:"),
    read(AtomMatricula),
    (
        ehAluno(AtomMatricula)-> 
            atom_string(AtomMatricula, Matricula),
            getAluno(Matricula, Aluno),
            member(Disciplina, Aluno.disciplinas),
            delete(Aluno.disciplinas, Disciplina , DisciplinasRestantes),
            atualizaAtributoAluno(Matricula, "removerDisciplina", DisciplinasRestantes),
            swritef(Out,"Aluno desvinculado da disciplina %w", [Disciplina]),
            writeln(Out);
        writeln("Aluno não existe no sistema.")
    ).
