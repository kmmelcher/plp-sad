-- O módulo ProfessorController fornece todas as funcionalidades necessárias de interação para lidar com o Tipo
-- Professor. Funciona como mediador de informações entre um controller e o Professor, além de interagir também
-- diretamente com o usuário.

module Controller.ProfessorController where
    import Util.TxtFunctions
    import Model.Ticket
    import Controller.AlunoController
    import Model.Professor as P

     
    -- Retorna o professor atravez de seu id.
    --  > Parametros:
    --    id = id do professor
    getProfessor:: Int -> IO P.Professor
    getProfessor id = do
        professorToString <- getObjetoById "Professores" id
        return (read professorToString :: P.Professor)

    
    -- Verifica se uma disciplina pertence ao professor.
    --  > Parametros:
    --    professor =  professor que contém as disciplinas a serem verificadas
    --    disciplina = disciplina a ser verificada
    ehDisciplinaDoProfessor :: P.Professor -> String -> Bool
    ehDisciplinaDoProfessor professor disciplina = disciplina `elem` P.disciplinas professor

    -- Solicita ao usuário a silga da disciplina e verifica se é válido.
    --  > Parametros:
    --    professor =  professor que contém as disciplinas a serem verificadas
    solicitaDisciplina :: P.Professor -> IO String
    solicitaDisciplina professor =
        if length (P.disciplinas professor) > 1 then do
            putStrLn "\nInforme a sigla da disciplina relacionada:"
            disciplina <- getLine
            if ehDisciplinaDoProfessor professor disciplina then return disciplina else do
                putStrLn "Insira uma sigla válida!\n"
                solicitaDisciplina professor
        else return (head (P.disciplinas professor))

    -- Cadastra um professor ao sistema do SAD, pegando os dados atráves de inputs.
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

    -- Verifica se a disciplina existe na database de disciplinas.
    --  > Parametros:
    --    (disciplinaAtual:disciplinasRestantes) = array de strings com disciplinas existentes
    --    disciplina = string contendo a sigla da disciplina a ser verificada
    verificaDisciplina :: [String] -> String -> Bool
    verificaDisciplina [] _ = False
    verificaDisciplina (disciplinaAtual:disciplinasRestantes) disciplina = do
        (disciplinaAtual == disciplina) || verificaDisciplina disciplinasRestantes disciplina