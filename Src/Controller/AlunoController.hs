module Src.Controller.AlunoController where
    import Src.Model.Aluno as A
    import Src.Util.TxtFunctions

    import Src.Controller.ChatController
    import Src.Model.Ticket


    import Src.Controller.DisciplinaController as DC
    

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

    matriculaAlunoEmDisciplina :: Aluno -> IO()
    matriculaAlunoEmDisciplina aluno = do
        putStrLn "Atualmente, estas são todas as disciplinas disponíveis para matrícula, exibidas em id, nome e sigla:\n\n"
        exibeDisciplinas aluno
        putStrLn "\nInforme a sigla da disciplina na qual deseja se matricular:"
        sigla <- getLine 
        -- CHECA SE SIGLA EXISTE
        let alunoAtualizado = Aluno (A.id aluno) (nome aluno) ([sigla] ++ disciplinas aluno)
        atualizaLinhaById "Alunos" (show (A.id aluno)) (show aluno)
        putStrLn "Matricula realizada com sucesso!"
