module Src.Controller.AlunoController where
    import Src.Model.Aluno as A
    import Src.Util.TxtFunctions
    import Src.Controller.ChatController
    import Src.Model.Ticket as T
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
        ticketsIds <- pegaTicketsDoAluno matAluno
        mostraTickets ticketsIds
        putStrLn "Escolha entre os seus Tickets qual será excluido: "
        sel <- getLine 
        if verificaTicket ticketsIds (read sel)
            then removeLinha "Tickets" sel
            else print "Ticket invalido"

    mostraTickets :: [Int] -> IO()
    mostraTickets [] = return ()
    mostraTickets (head:tail) = do
        ticketStr <- buscaObjetoById "Tickets" head
        let ticket = read ticketStr :: Ticket
        putStrLn $ show (T.id ticket) ++ ") " ++ titulo ticket ++ " (" ++ status ticket ++ ")"
        mostraTickets tail

    verificaTicket :: [Int] -> Int -> Bool
    verificaTicket [] x = False
    verificaTicket (head:tail) x = do
        (head == x) || verificaTicket tail x

    matriculaAlunoEmDisciplina :: Aluno -> IO()
    matriculaAlunoEmDisciplina aluno = do
        putStrLn "Você já está matriculado nas seguintes disciplinas: "
        putStrLn(show (disciplinas aluno))
        putStrLn "Atualmente, estas são todas as disciplinas disponíveis para matrícula, exibidas em id, nome e sigla:\n\n"
        exibeDisciplinas aluno
        putStrLn "\nInforme a sigla da disciplina na qual deseja se matricular:"
        sigla <- getLine
        siglasCadastradas <- retornarTodasSiglas
        if sigla `elem` siglasCadastradas then
            if sigla `elem` (disciplinas aluno) then do 
                putStrLn ("Você já está matriculado em " ++ sigla ++ " , tente novamente\n\n")
                matriculaAlunoEmDisciplina aluno 
            else do 
                let alunoAtualizado = Aluno (A.id aluno) (nome aluno) ([sigla] ++ disciplinas aluno)
                atualizaLinhaById "Alunos" (show (A.id aluno)) (show alunoAtualizado)
                putStrLn "Matricula realizada com sucesso!"
        else do 
            putStrLn "Sigla Inválida , tente novamente\n\n"
            matriculaAlunoEmDisciplina aluno
    
    desmatriculaAlunoDeDisciplina :: Aluno -> IO()
    desmatriculaAlunoDeDisciplina aluno = do 
        putStrLn "\nAtualmente, o aluno está matriculado nas seguintes disciplinas: \n"
        putStrLn (show(disciplinas aluno))
        putStrLn "Informe a sigla da disciplina na qual deseja se desmatricular: "
        sigla <- getLine 
        if sigla `elem` (disciplinas aluno) then do
            let disciplinasExcetoMencionada = filter (/= sigla) (disciplinas aluno)
            let alunoAtualizado = Aluno (A.id aluno) (nome aluno) (disciplinasExcetoMencionada)
            atualizaLinhaById "Alunos" (show (A.id aluno)) (show alunoAtualizado)
            putStrLn "Cancelamento de matricula realizada com sucesso!\n"
        else do 
            putStrLn ("Você não está matriculado na disciplina " ++ sigla ++ " . Tente novamente\n\n")
            desmatriculaAlunoDeDisciplina aluno
    
    resolveTicket :: Aluno -> IO()
    resolveTicket aluno = do
        ticketsAluno <- pegaTicketsDoAluno (A.id aluno)
        putStrLn "Estes são os seus tickets com status \"Em andamento\"\n"
        exibeTicketsEmAndamento ticketsAluno
        putStrLn "\ninsira o id do ticket que deseja marcar como concluído"
        input <- getLine
        let id = read input :: Int
        idValido <- checaIdDeTicketEmAndamento id
        if idValido then do
            marcaTicketComoConcluido id
            putStrLn "Ticket alterado com sucesso.\n"
             else do
            putStrLn "Insira um id válido!"
            resolveTicket aluno

    adicionaMensagemAluno :: Aluno -> IO()
    adicionaMensagemAluno aluno = do
        putStrLn "Foram identificados os seguintes tickets desse autor: "
        tickets <- pegaTicketsDoAluno (A.id aluno)
        print tickets
        adicionaMensagem (A.id aluno)
