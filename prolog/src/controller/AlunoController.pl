:- module('AlunoController', [getAluno/2, ehAluno/1]).

:- use_module('../util/jsonFunctions.pl', [getObjetoByID/3, atualizaAtributoAluno/3, checaExistencia/2]).

getAluno(Id, Aluno):-
    getObjetoByID("alunos", Id, AlunoJson),
    split_string(AlunoJson.disciplinas, ",", "", DisciplinasFormated),
    put_dict([disciplinas=DisciplinasFormated], AlunoJson, Aluno).


ehAluno(Id):- checaExistencia("alunos", Id).

vinculaAlunoDisciplina(Id, Disciplina):-
    getAluno(Id, Aluno),
    DisciplinasAntigas = Aluno.disciplinas,
    \+ member(Disciplina, DisciplinasAntigas),
    append(DisciplinasAntigas, [Disciplina], DisciplinasNovas),
    atualizaAtributoAluno(Id, "disciplinas", DisciplinasNovas).
