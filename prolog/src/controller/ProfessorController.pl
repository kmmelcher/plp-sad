:- module('ProfessorController', [getProfessor/2, ehProfessor/1]).

:- use_module('../util/jsonFunctions.pl', [getObjetoByID/3, checaExistencia/2]).

getProfessor(Id, Professor):-
    getObjetoByID("professores", Id, ProfessorJson),
    split_string(ProfessorJson.disciplinas, ",", "", DisciplinasFormated),
    put_dict([disciplinas=DisciplinasFormated], ProfessorJson, Professor).

ehProfessor(Id):- checaExistencia("professores", Id).
