module Controller.DisciplinaController where
    import Model.Disciplina as D
    import Util.TxtFunctions
    import Model.Aluno as A

    getDisciplina:: Int -> IO Disciplina
    getDisciplina id = do
        disciplinaToString <- getObjetoById "Disciplinas" id
        return (read disciplinaToString :: Disciplina)

    {- 
    Mostra as disciplinas em que um aluno ainda pode se matricular
    Parametros:
        aluno = aluno que será analisado
    -}
    exibeDisciplinasDisponiveis:: Aluno -> IO()
    exibeDisciplinasDisponiveis aluno = do
        disciplinas <- fileToStringArray "Disciplinas"
        exibeDisciplinasDisponiveisRecursivo disciplinas aluno

    {- 
    Função auxiliar de exibeDiciplinasDisponiveis
    -}
    exibeDisciplinasDisponiveisRecursivo :: [String] -> Aluno  -> IO()
    exibeDisciplinasDisponiveisRecursivo [] _ = return ()
    exibeDisciplinasDisponiveisRecursivo (disciplinaAtual:disciplinasRestantes) aluno = do
        let disciplina = (read disciplinaAtual :: Disciplina)
        if show (D.sigla disciplina) `elem` disciplinas aluno
            then exibeDisciplinasDisponiveisRecursivo disciplinasRestantes aluno else do
            putStrLn (show (D.id disciplina) ++ ") " ++ show (D.nome disciplina) ++ " - " ++ show (sigla disciplina))
            exibeDisciplinasDisponiveisRecursivo disciplinasRestantes aluno
    
    {- 
    Retorna as siglas de todas as disciplinas
    Parametros:
    -}
    getSiglas :: IO([String])
    getSiglas = do
        disciplinas <- fileToStringArray "Disciplinas"
        getSiglasRecursivo disciplinas []

    {- 
    Função auxliar de getSiglas
    -}
    getSiglasRecursivo :: [String] -> [String] -> IO[String]
    getSiglasRecursivo [] ret = return ret
    getSiglasRecursivo (disciplinaAtual:disciplinasRestantes) ret = do
        let disciplina = (read disciplinaAtual :: Disciplina)
        getSiglasRecursivo disciplinasRestantes (D.sigla disciplina : ret)
        