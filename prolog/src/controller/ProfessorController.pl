:- module('ProfessorController', [getProfessor/2]).

:- use_module('../util/jsonFunctions.pl', [getObjetoByID/3]).

getProfessor(Id, Professor):-
    getObjetoByID("professores", Id, Professor).