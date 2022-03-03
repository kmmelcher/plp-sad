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
    Adiciona um professor ao sistema atravez de entradas do usuario
    -}
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

    {- 
    Verifica se uma disciplina está presente num grupo de disciplinas
    Parametros:
        disciplinas = grupo de disciplinas
        disciplina = disciplina a ser verificada
    -}
    verificaDisciplina :: [String] -> String -> Bool
    verificaDisciplina [] _ = False
    verificaDisciplina (disciplinaAtual:disciplinasRestantes) disciplina = do
        (disciplinaAtual == disciplina) || verificaDisciplina disciplinasRestantes disciplina