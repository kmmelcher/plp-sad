:- include("../util/jsonFunctions.pl").

getAluno(Id, Aluno):-
    (checaExistencia("alunos", Id) -> getObjetoByID("alunos", Id, Aluno); 
    Aluno = "Inexistente").