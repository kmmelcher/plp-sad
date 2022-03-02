module Src.Controller.MonitorController where
    import Src.Model.Monitor as M
    import Src.Util.TxtFunctions
    import Src.Model.Ticket as T
    import Src.Controller.ChatController
    
    adicionaMonitor :: IO()
    adicionaMonitor = do
        id <- insereMatricula
        disciplina <- insereDisciplina id
        if disciplina == "VOLTAR"
            then putStrLn "Cadastro cancelado.\n"
        else do
            putStrLn "Insira os horários de atendimento do monitor"
            horarios <- getLine
            let monitor = Monitor id disciplina horarios
            adicionaLinha "Monitores" $ show monitor
            putStrLn "Monitor cadastrado com sucesso!\n"

    insereMatricula :: IO(Int)
    insereMatricula = do
        putStrLn "\nInsira a matricula do monitor"
        id <- readLn 
        alunoCadastrado <- checaExistenciaById "Alunos" id
        if alunoCadastrado || id == 0
            then return id
        else do
            putStrLn "\nAluno não cadastrado!\n(Digite 0 para voltar ao menu principal)\n"
            insereMatricula

    insereDisciplina :: Int -> IO(String)
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

    removeMonitor :: [String] -> IO()
    removeMonitor disciplinasDoProfessor = do
        id <- insereMatricula
        monitorToString <- buscaObjetoById "Monitores" id
        let monitor = read monitorToString :: Monitor

        if (M.disciplina monitor) `elem` disciplinasDoProfessor then do
            removeLinha "Monitores" (show id)
            putStrLn "Monitor removido com sucesso!\n"
            else do
                putStrLn "Este monitor não é de uma disciplina sua! Tente novamente."
                removeMonitor disciplinasDoProfessor

    respondeTicket :: Monitor -> IO()
    respondeTicket monitor = do
        tickets <- getTicketsEmAndamento (M.disciplina monitor)
        if tickets == [] then putStrLn "Não há tickets em andamento por enquanto." else do
            putStrLn "\nTickets em andamento da sua disciplina:"
            exibeTickets tickets
            adicionaMensagem (M.id monitor)
