module Controller.MonitorController where
    import Model.Monitor as M
    import Util.TxtFunctions
    import Controller.AlunoController

    getMonitor:: Int -> IO Monitor
    getMonitor id = do
        monitorToString <- getObjetoById "Monitores" id
        return (read monitorToString :: Monitor)
    
    {- 
    Adiciona um monitor a partir de dados inseridos pelo usuário
    -}
    vinculaMonitor :: String -> IO()
    vinculaMonitor disciplina = do
        putStrLn "Insira a matricula do aluno (digite 0 para voltar ao seu menu):"
        id <- readLn
        ehAluno <- checaExistenciaById "Alunos" id
        alunoCursaDisciplina <- alunoCursaDisciplina id disciplina
        ehMonitor <- checaExistenciaById "Monitores" id
        analisaVinculoMonitor ehAluno alunoCursaDisciplina ehMonitor id disciplina     
    
    analisaVinculoMonitor :: Bool -> Bool -> Bool -> Int -> String -> IO()
    analisaVinculoMonitor ehAluno alunoCursaDisciplina ehMonitor id disciplina
        | id == 0 = return ()
        | not ehAluno = putStrLn "Este aluno não está cadastrado!\n"
        | alunoCursaDisciplina = putStrLn "Este aluno está cursando sua disciplina! Tente novamente\n"
        | ehMonitor = putStrLn "Este monitor já esta vinculado a uma disciplina!\n"
        | otherwise = do
            putStrLn "Insira os horários de atendimento do monitor"
            horarios <- getLine
            let monitor = Monitor id disciplina horarios
            adicionaLinha "Monitores" $ show monitor
            putStrLn "Monitor cadastrado com sucesso!\n"

    desvinculaMonitor :: String -> IO()
    desvinculaMonitor disciplina = do
        putStrLn "Informe a matrícula do monitor a ser desvinculado de sua disciplina (ou digite 0 para voltar ao seu menu):"
        id <- readLn
        ehAluno <- checaExistenciaById "Alunos" id
        ehMonitor <- checaExistenciaById "Monitores" id
        ehMonitorDaDisciplina <- analisaAlunoComoMonitor id disciplina
        analisaDesvinculoMonitor ehAluno ehMonitor ehMonitorDaDisciplina id disciplina
    
    analisaDesvinculoMonitor :: Bool -> Bool -> Bool -> Int -> String -> IO()
    analisaDesvinculoMonitor ehAluno ehMonitor ehMonitorDaDisciplina id disciplina
        | id == 0 = return () 
        | not ehAluno = putStrLn "Esta matrícula não está cadastrada!\n"
        | not ehMonitor = putStrLn "Este monitor não está cadastrado!\n"
        | not ehMonitorDaDisciplina = putStrLn "Este monitor não é da disciplina informada!\n"
        | otherwise = do
            removeLinha "Monitores" (show id)
            putStrLn "Monitor desvinculado com sucesso!\n"

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