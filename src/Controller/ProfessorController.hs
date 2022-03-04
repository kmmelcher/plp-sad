module Controller.ProfessorController where
    import Util.TxtFunctions
    import Model.Ticket
    import Controller.AlunoController
    import Model.Professor as P

    {- 
    Retorna o professor atravez de seu id
    Parametros:
        id = id do professor
    -}
    getProfessor:: Int -> IO P.Professor
    getProfessor id = do
        professorToString <- getObjetoById "Professores" id
        return (read professorToString :: P.Professor)

    {- 
    Verifica se uma disciplina pertence ao professor
    Parametros:
        professor = objeto professor que coném as disciplinas em questão
        disciplina = disciplina a ser verificada
    -}
    ehDisciplinaDoProfessor :: P.Professor -> String -> Bool
    ehDisciplinaDoProfessor professor disciplina = disciplina `elem` P.disciplinas professor

    solicitaDisciplina :: P.Professor -> IO String
    solicitaDisciplina professor =
        if length (P.disciplinas professor) > 1 then do
            putStrLn "\nInforme a sigla da disciplina relacionada:"
            disciplina <- getLine
            if ehDisciplinaDoProfessor professor disciplina then return disciplina else do
                putStrLn "Insira uma sigla válida!\n"
                solicitaDisciplina professor
        else return (head (P.disciplinas professor))

    adicionaProfessor :: IO()
    adicionaProfessor = do
            putStrLn "Insira o nome do professor: "
            nome <- getLine
            putStrLn "Insira o nome das disciplinas do professor: "
            listaDisciplinasStr <- getLine
            let disciplinas = read listaDisciplinasStr :: [String]
            id <- buscaNovoId "Professores"
            let prof = P.Professor (read id :: Int) nome disciplinas
            let profToString = show prof
            adicionaLinha "Professores" profToString
            putStrLn ("Professor cadastrado com sucesso no id " ++ id ++ ". Decore seu id para utilizar o sistema!\n")

    verificaDisciplina :: [String] -> String -> Bool
    verificaDisciplina [] _ = False
    verificaDisciplina (disciplinaAtual:disciplinasRestantes) disciplina = do
        (disciplinaAtual == disciplina) || verificaDisciplina disciplinasRestantes disciplina