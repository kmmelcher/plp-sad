module Src.Controller.ProfessorController where
    import Src.Model.Professor
    import Src.Util.TxtFunctions
    import Src.Controller.ChatController

    getProfessor:: Int -> IO Professor
    getProfessor id = do
        professorToString <- getObjetoById "Professores" id
        return (read professorToString :: Professor)

    adicionaProfessor :: IO()
    adicionaProfessor = do
            putStrLn "Insira o nome do professor: "
            nome <- getLine
            putStrLn "Insira o nome das disciplinas do professor: "
            listaDisciplinasStr <- getLine
            let disciplinas = read listaDisciplinasStr :: [String]
            id <- buscaNovoId "Professores"
            let prof = Professor (read id :: Int) nome disciplinas
            let profToString = show prof
            adicionaLinha "Professores" profToString
            putStrLn ("Professor cadastrado com sucesso no id " ++ id ++ ". Decore seu id para utilizar o sistema!\n")

    lerTicketsDisciplina :: Professor -> IO()
    lerTicketsDisciplina professor = do
        if length (disciplinas professor) > 1 then do
            putStrLn "Insira qual disciplina você deseja visualizar os tickets:"
            disciplina <- getLine
            if verificaDisciplina (disciplinas professor) disciplina
                then exibeTicketsDisciplina disciplina
                else do
                    putStrLn "\nDisciplina invalida!"
                    lerTicketsDisciplina professor
        else do
               exibeTicketsDisciplina (head (disciplinas professor))

    verificaDisciplina :: [String] -> String -> Bool
    verificaDisciplina [] _ = False
    verificaDisciplina (disciplinaAtual:disciplinasRestantes) disciplina = do
        (disciplinaAtual == disciplina) || verificaDisciplina disciplinasRestantes disciplina