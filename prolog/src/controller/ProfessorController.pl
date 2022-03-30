:- module('ProfessorController', [getProfessor/2, ehProfessor/1]).

:- use_module('../util/jsonFunctions.pl', [getObjetoByID/3, checaExistencia/2]).

getProfessor(Id, Professor):-
    getObjetoByID("professores", Id, Professor).

ehProfessor(Id):- checaExistencia("professores", Id).