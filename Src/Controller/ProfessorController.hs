module Src.Controller.ProfessorController where
    import Src.Model.Professor
    import Src.Util.TxtFunctions
    
    {- 
    Retorna o professor atravez de seu id
    Parametros:
        id = id do professor
    -}
    getProfessor:: Int -> IO Professor
    getProfessor id = do
        professorToString <- getObjetoById "Professores" id
        return (read professorToString :: Professor)

    {- 
    Verifica se uma disciplina está presente num grupo de disciplinas
    Parametros:
        disciplinas = grupo de disciplinas
        disciplina = disciplina a ser verificada
    -}
    ehDisciplinaDoProfessor :: Professor -> String -> Bool
    ehDisciplinaDoProfessor professor disciplina = disciplina `elem` (disciplinas professor)
    
    solicitaDisciplina :: Professor -> IO(String)
    solicitaDisciplina professor = 
        if (length (disciplinas professor) > 1) then do
            putStrLn "Informe a sigla da disciplina relacionada:"
            disciplina <- getLine
            if ehDisciplinaDoProfessor professor disciplina then return disciplina else do
                putStrLn "Insira uma sigla válida!\n"
                solicitaDisciplina professor
        else return ((disciplinas professor)!!0)