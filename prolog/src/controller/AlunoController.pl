:- module('AlunoController', [getAluno/2]).

:- include("../util/jsonFunctions.pl").

getAluno(Id, Aluno):-
    checaExistencia("alunos", Id),
    getObjetoByID("alunos", Id, Aluno).

vinculaAlunoDisciplina(Id, Disciplina):-
    getAluno(Id, Aluno),
    DisciplinasAntigas = Aluno.disciplinas,
    \+ member(Disciplina, DisciplinasAntigas),
    append(DisciplinasAntigas, [Disciplina], DisciplinasNovas),
    atualizaAtributoAluno(Id, "disciplinas", DisciplinasNovas).
