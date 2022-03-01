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
        putStrLn "Insira a matricula do monitor"
        id <- readLn 
        alunoCadastrado <- checaExistenciaById "Alunos" id
        if alunoCadastrado || id == 0
            then return id
        else do
            putStrLn "Aluno não cadastrado!\n(Digite 0 para voltar ao menu principal)\n"
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

    exibeTicketsDisciplina :: Monitor -> IO()
    exibeTicketsDisciplina monitor = do
        tickets <- pegaTicketsDeUmaDisciplina (M.disciplina monitor)
        if tickets == [] then putStrLn "\nAinda não há tickets nesta disciplina.\n" else do
            putStrLn ("\nEstes são os tickets existentes da disciplina: " ++ M.disciplina monitor)
            exibeTicketsDisciplinaRecursivo tickets
    
    exibeTicketsDisciplinaRecursivo :: [Int] -> IO()
    exibeTicketsDisciplinaRecursivo [] = do
        putStr "\n"
        return ()
    exibeTicketsDisciplinaRecursivo (idTicketAtual:idsTicketsRestantes) = do
        instanciaTicket <- buscaObjetoById "Tickets" idTicketAtual
        let ticket = read instanciaTicket :: Ticket
        putStrLn (show (T.id ticket) ++ ") " ++ titulo ticket ++ " (" ++ status ticket ++ ")")
        exibeTicketsDisciplinaRecursivo idsTicketsRestantes
