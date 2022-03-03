module Src.Controller.MonitorController where
    import Src.Model.Monitor as M
    import Src.Util.TxtFunctions
    import Src.Controller.AlunoController
    
    {- 
    Retorna o monitor a partir do seu ID
    Parametros:
        id = matricula do monitor
    -}
    getMonitor:: Int -> IO Monitor
    getMonitor id = do
        monitorToString <- getObjetoById "Monitores" id
        return (read monitorToString :: Monitor)
    
    {- 
    Adiciona um monitor a partir de dados inseridos pelo usuaário
    -}
    adicionaMonitor :: String -> IO()
    adicionaMonitor disciplina = do
        putStrLn "Insira a matricula do aluno (digite 0 para voltar ao seu menu):"
        id <- readLn
        ehAluno <- checaExistenciaById "Alunos" id
        alunoCursaDisciplina <- alunoCursaDisciplina id disciplina
        ehMonitor <- checaExistenciaById "Monitores" id
        if id == 0 then return ()
        else if not ehAluno then do
            putStrLn "Este aluno não está cadastrado! Tente novamente\n"
            adicionaMonitor disciplina
        else if alunoCursaDisciplina then do
            putStrLn "Este aluno está cursando sua disciplina! Tente novamente\n"
        else if ehMonitor then do
            putStrLn "Este monitor já esta vinculado a uma disciplina!\n"
            adicionaMonitor disciplina
        else do
            putStrLn "Insira os horários de atendimento do monitor"
            horarios <- getLine
            let monitor = Monitor id disciplina horarios
            adicionaLinha "Monitores" $ show monitor
            putStrLn "Monitor cadastrado com sucesso!\n"

    {- 
    Realiza as entradas do usuario para receber uma matricula de monitor válida
    -}
    insereMatricula :: IO Int
    insereMatricula = do
        putStrLn "\nInsira a matricula do monitor"
        id <- readLn
        alunoCadastrado <- checaExistenciaById "Alunos" id
        monitorCadastrado <- checaExistenciaById "Monitores" id
        if (alunoCadastrado && monitorCadastrado) || id == 0
            then return id
        else do
            putStrLn "\nMonitor não identificado, tente novamente.\n(Digite 0 para voltar ao seu menu)"
            insereMatricula

    {- 
    função para receber os dados da disciplina de um monitor no momento do cadastro
    Parametros:
        id = matricula do monitor
    -}
    insereDisciplina :: Int -> IO String
    insereDisciplina id = do
        if id == 0
            then return "VOLTAR"
        else do
            putStrLn "Insira a disciplina do monitor"
            disciplina <- getLine
            disciplinaCadastrada <- checaExistenciaByAtributo  "Disciplinas" "sigla" (adicionaAspas disciplina)
            if disciplinaCadastrada || disciplina  == "VOLTAR"
                then return disciplina
            else do
                putStrLn "Disciplina não cadastrada!\n(Digite \"VOLTAR\" para voltar ao menu principal)\n"
                insereDisciplina id

    {- 
    Função que remove um monitor a partir de entradas do usuario
    Parametros:
        disciplinasDoProfessor = array com as disciplinas do professor que deseja remover o monitor
    -}
    removeMonitor :: [String] -> IO()
    removeMonitor disciplinasDoProfessor = do
        id <- insereMatricula
        monitor <- getMonitor id
        if M.disciplina monitor `elem` disciplinasDoProfessor then do
            removeLinha "Monitores" (show id)
            putStrLn "Monitor removido com sucesso!\n"
            else do
                putStrLn "Este monitor não é de uma disciplina sua! Tente novamente."
                removeMonitor disciplinasDoProfessor