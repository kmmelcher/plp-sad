-- O módulo DisciplinaController fornece todas as funcionalidades necessárias de interação para lidar com o Tipo
-- Disciplina. Funciona como mediador de informações entre um controller e a Disciplina.

module Controller.DisciplinaController where
    import Model.Disciplina as D
    import Util.TxtFunctions
    import Model.Aluno as A

    -- Função que retorna uma Disciplina com base no id dela.
    --  > Parametros:
    --    id = id da disciplina
    getDisciplina:: Int -> IO Disciplina
    getDisciplina id = do
        disciplinaToString <- getObjetoById "Disciplinas" id
        return (read disciplinaToString :: Disciplina)
 
    -- Mostra as disciplinas em que um aluno ainda pode se matricular.
    --  > Parametros:
    --    aluno = aluno que será analisado
    exibeDisciplinasDisponiveis:: Aluno -> IO()
    exibeDisciplinasDisponiveis aluno = do
        disciplinas <- fileToStringArray "Disciplinas"
        exibeDisciplinasDisponiveisRecursivo disciplinas aluno

     
    -- Função recursiva, responsável por estruturar a mensagem com as disciplinas disponíveis e mostrá-las
    -- ao usuário.
    --  > Parametros:
    --    (disciplinaAtual:disciplinasRestantes) = array de string contendo todas as disciplinas existentes
    --    aluno = aluno que será analisado
    exibeDisciplinasDisponiveisRecursivo :: [String] -> Aluno  -> IO()
    exibeDisciplinasDisponiveisRecursivo [] _ = return ()
    exibeDisciplinasDisponiveisRecursivo (disciplinaAtual:disciplinasRestantes) aluno = do
        let disciplina = (read disciplinaAtual :: Disciplina)
        if show (D.sigla disciplina) `elem` disciplinas aluno
            then exibeDisciplinasDisponiveisRecursivo disciplinasRestantes aluno else do
            putStrLn (show (D.id disciplina) ++ ") " ++ show (D.nome disciplina) ++ " - " ++ show (sigla disciplina))
            exibeDisciplinasDisponiveisRecursivo disciplinasRestantes aluno
    

    -- Retorna as siglas de todas as disciplinas
    getSiglas :: IO([String])
    getSiglas = do
        disciplinas <- fileToStringArray "Disciplinas"
        getSiglasRecursivo disciplinas []

     
    -- Função recursiva, responsável por construir o array de string contendo as siglas e retorná-las.
    --  > Parametros:
    --    (disciplinaAtual:disciplinasRestantes) = array de string contendo todas as disciplinas existentes
    --    result = array de string o qual se deseja concatenar o resultado 
    getSiglasRecursivo :: [String] -> [String] -> IO[String]
    getSiglasRecursivo [] result = return result
    getSiglasRecursivo (disciplinaAtual:disciplinasRestantes) result = do
        let disciplina = (read disciplinaAtual :: Disciplina)
        getSiglasRecursivo disciplinasRestantes (D.sigla disciplina : result)
        