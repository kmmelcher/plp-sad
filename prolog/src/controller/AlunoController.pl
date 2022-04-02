:- module('AlunoController', [getAluno/2, ehAluno/1, vinculaAlunoDisciplina/1, removeAluno/1, desvinculaAlunoDisciplina/1]).
:- use_module('../util/jsonFunctions.pl', [getObjetoByID/3, atualizaAtributoAluno/3, checaExistencia/2, addAluno/4, removeAluno/1]).
:- use_module('../controller/MonitorController.pl', [ehMonitor/1, getMonitor/2]).
:- use_module('../util/input.pl',[input/1]).
:- use_module('../util/EncriptFunctions.pl',[encripta/3]).

getAluno(Id, Aluno):-
    getObjetoByID("alunos", Id, AlunoJson),
    split_string(AlunoJson.disciplinas, ",", "", DisciplinasFormated),
    put_dict([disciplinas=DisciplinasFormated], AlunoJson, Aluno).

ehAluno(Id):- checaExistencia("alunos", Id).

vinculaAlunoDisciplina(Disciplina):-
    writeln("Digite a matricula do Aluno :"),
    input(IdAtom), atom_string(IdAtom, Id),
    (
        ehAluno(Id) -> 
            getAluno(Id, Aluno),
            (
                ehMonitor(Id), getMonitor(Id, Monitor), Disciplina = Monitor.disciplina -> 
                    writeln("Este aluno eh monitor de sua disciplina!")
                ;
                (
                    member(Disciplina, Aluno.disciplinas) -> 
                        writeln("Este aluno ja esta nesta disciplina");
                        atualizaAtributoAluno(Aluno.id, "disciplinas", Disciplina),
                        writeln("Aluno vinculado com sucesso.")
                )
                
            );
        writeln("\nEste aluno nao esta cadastrado no SAD.\n"),
        cadastraAluno("", Id, Disciplina)   
    ).

cadastraAluno(Nome, Matricula, Disciplina):-
    (Nome = "" -> 
        writeln("Digite o nome do ingressante: "),
        input(NomeAluno); NomeAluno = Nome),
    
    (Disciplina = "" ->
            writeln("Digite a disciplina do ingressante: "),
            input(DisciplinaAtom), atom_string(DisciplinaAtom,DisciplinaAluno);
            DisciplinaAluno = Disciplina
    ),
    (Matricula = "" ->
        writeln("Digite a nova matricula do ingressante: "),
        input(MatriculaAtom),atom_string(MatriculaAtom,MatriculaAluno);
            MatriculaAluno = Matricula
        ),
    encripta('aluno', NomeAluno, SenhaEncriptada),    
    addAluno(MatriculaAluno, NomeAluno, [DisciplinaAluno], SenhaEncriptada), 
    swritef(Out, "\nAluno %w criado com matricula:%w e senha padrão:aluno", [Nome, Matricula]),
    writeln(Out). 


removerAluno(Id):-
    removeAluno(Id),
    writeln("Aluno removido com sucesso!").

desvinculaAlunoDisciplina(Disciplina) :-
    writeln("Matricula:"),
    input(AtomMatricula),
    (
        ehAluno(AtomMatricula)-> 
            atom_string(AtomMatricula, Matricula),
            getAluno(Matricula, Aluno),
            (
                \+member(Disciplina, Aluno.disciplinas) -> 
                    writeln("Este aluno nao esta vinculado a sua disciplina");
                delete(Aluno.disciplinas, Disciplina , DisciplinasRestantes),
                atualizaAtributoAluno(Matricula, "removerDisciplina", DisciplinasRestantes),
                swritef(Out,"Aluno desvinculado da disciplina %w", [Disciplina]),
                writeln(Out)
            );
            
        writeln("Aluno nao existe no sistema.")
    ).
    