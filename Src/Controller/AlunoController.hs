module Src.Controller.AlunoController where
    import Src.Model.Aluno
    import Src.Util.TxtFunctions
    import Src.Controller.ChatController
    import Src.Model.Ticket

    adicionaAluno :: IO()
    adicionaAluno = do
        putStrLn "Insira o nome do aluno: "
        nome <- getLine
        putStrLn "Insira a matricula do aluno"
        matricula <- readLn
        putStrLn "Insira as disciplinas do aluno"
        disciplinas <- readLn
        let aluno = Aluno matricula nome disciplinas
        adicionaLinha "Alunos" $ show aluno
        putStrLn "Aluno cadastrado com sucesso.\n"

    excluirTicket :: Int -> IO()
    excluirTicket matAluno = do
        putStrLn "Escolha entre os seus Tickets qual será excluido :"
        ticketsIds <- pegaTicketsDoAluno matAluno
        mostraTickets ticketsIds
        sel <- getLine 
        if verificaTicket ticketsIds (read sel)
            then removeLinha "Tickets" sel
            else print "Ticket invalido"

    mostraTickets :: [Int] -> IO()
    mostraTickets [] = print "end"
    mostraTickets (head:tail) = do
        ticketStr <- buscaObjetoById "Tickets" head
        let ticket = read ticketStr :: Ticket
        putStrLn $ show (Src.Model.Ticket.id ticket) ++ ": " ++ titulo ticket
        mostraTickets tail

    verificaTicket :: [Int] -> Int -> Bool
    verificaTicket [] x = False
    verificaTicket (head:tail) x = do
        (head == x) || verificaTicket tail x