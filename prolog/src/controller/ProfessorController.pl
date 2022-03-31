:- module('ProfessorController', [getProfessor/2]).

:- use_module('../util/jsonFunctions', [getObjetoByID/3]).

getProfessor(Id, Professor):-
    getObjetoByID("professores", Id, ProfessorJson),
    split_string(ProfessorJson.disciplinas, ",", "", DisciplinasFormated),
    put_dict([disciplinas=DisciplinasFormated], ProfessorJson, Professor).
