-- O módulo MonitorController fornece todas as funcionalidades necessárias de interação para lidar com o Tipo
-- Monitor. Funciona como mediador de informações entre um controller e o Monitor, além de interagir também
-- diretamente com o usuário.

module Controller.MonitorController where
    import Model.Monitor as M
    import Util.TxtFunctions
    import Controller.AlunoController

    -- Função que retorna um Monitor com base no id dele.
    --  > Parametros:
    --    id = matricula do monitor
    getMonitor:: Int -> IO Monitor
    getMonitor id = do
        monitorToString <- getObjetoById "Monitores" id
        return (read monitorToString :: Monitor)
    

    -- Adiciona um monitor a partir de dados inseridos pelo usuário.
    --  > Parametros:
    --    disciplina = string contendo a sigla da disciplina o qual o monitor será vinculado
    vinculaMonitor :: String -> IO()
    vinculaMonitor disciplina = do
        putStrLn "Insira a matricula do aluno (digite 0 para voltar ao seu menu):"
        id <- readLn
        ehAluno <- checaExistenciaById "Alunos" id
        alunoCursaDisciplina <- alunoCursaDisciplina id disciplina
        ehMonitor <- checaExistenciaById "Monitores" id
        analisaVinculoMonitor ehAluno alunoCursaDisciplina ehMonitor id disciplina     

    -- Verifica se é possível cadastrar o aluno como monitor corretamente.
    --  > Parametros:
    --    ehAluno = booleano que indica se o usuario é um aluno
    --    alunoCursaDisciplina = booleano que indica se o aluno cursa a disciplina que ele pretende se vincular
    --    ehMonitor = boolenao que indica se um aluno já é um monitor.
    --    id = matricula do aluno
    --    disciplina = string contendo a sigla da disciplina o qual o monitor será vinculado.   
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

    -- Desvincula o monitor de uma disciplina, a partir dos dados inseridos pelo usuário.
    --  > Parametros:
    --    disciplina = string contendo a sigla da disciplina o qual o monitor será vinculado. 
    desvinculaMonitor :: String -> IO()
    desvinculaMonitor disciplina = do
        putStrLn "Informe a matrícula do monitor a ser desvinculado de sua disciplina (ou digite 0 para voltar ao seu menu):"
        id <- readLn
        ehAluno <- checaExistenciaById "Alunos" id
        ehMonitor <- checaExistenciaById "Monitores" id
        ehMonitorDaDisciplina <- analisaAlunoComoMonitor id disciplina
        analisaDesvinculoMonitor ehAluno ehMonitor ehMonitorDaDisciplina id disciplina
    
    -- Verifica se é possível descadastrar o monitor de uma disciplina.
    --  > Parametros:
    --    ehAluno = booleano que indica se o usuario é um aluno
    --    ehMonitor = boolenao que indica se um aluno já é um monitor.
    --    ehMonitorDaDisciplina = booleano que indica se ele é monitor da disciplina.
    --    id = matricula do monitor
    --    disciplina = string contendo a sigla da disciplina o qual o monitor será vinculado. 
    analisaDesvinculoMonitor :: Bool -> Bool -> Bool -> Int -> String -> IO()
    analisaDesvinculoMonitor ehAluno ehMonitor ehMonitorDaDisciplina id disciplina
        | id == 0 = return () 
        | not ehAluno = putStrLn "Esta matrícula não está cadastrada!\n"
        | not ehMonitor = putStrLn "Este monitor não está cadastrado!\n"
        | not ehMonitorDaDisciplina = putStrLn "Este monitor não é da disciplina informada!\n"
        | otherwise = do
            removeLinha "Monitores" (show id)
            putStrLn "Monitor desvinculado com sucesso!\n"

    
    -- Valida as entradas do usuário, para a matrícula do monitor.
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

    -- Função para receber os dados da disciplina de um monitor no momento do cadastro.
    --  > Parametros:
    --    id = matricula do monitor
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