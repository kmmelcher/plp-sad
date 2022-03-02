module Src.Controller.AlunoController where
    import Src.Model.Aluno as A
    import Src.Util.TxtFunctions
    import Src.Controller.ChatController
    import Src.Model.Ticket as T
    import Src.Controller.DisciplinaController as DC
    
    getAluno:: Int -> IO(Aluno)
    getAluno id = do
        alunoToString <- getObjetoById "Alunos" id
        return (read alunoToString :: Aluno)

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

    -- DEVE SER MOVIDO PARA OUTRO CONTROLADOR
    excluirTicket :: Int -> IO()
    excluirTicket matAluno = do
        ticketsIds <- getTicketsAluno matAluno
        mostraTickets ticketsIds
        putStrLn "Escolha entre os seus Tickets qual será excluido: "
        sel <- getLine 
        if verificaTicket ticketsIds (read sel)
            then removeLinha "Tickets" sel
            else print "Ticket invalido"
    
    -- DEVE SER MOVIDO PARA OUTRO CONTROLADOR
    mostraTickets :: [Int] -> IO()
    mostraTickets [] = return ()
    mostraTickets (head:tail) = do
        ticketStr <- getObjetoById "Tickets" head
        let ticket = read ticketStr :: Ticket
        putStrLn $ show (T.id ticket) ++ ") " ++ titulo ticket ++ " (" ++ status ticket ++ ")"
        mostraTickets tail

    -- DEVE SER MOVIDO PARA OUTRO CONTROLADOR
    verificaTicket :: [Int] -> Int -> Bool
    verificaTicket [] x = False
    verificaTicket (head:tail) x = do
        (head == x) || verificaTicket tail x

    matriculaAlunoEmDisciplina :: Aluno -> IO()
    matriculaAlunoEmDisciplina aluno = do
        putStrLn "Você está matriculado nas seguintes disciplinas:"
        putStrLn (show (disciplinas aluno))
        putStrLn "Estas são todas as disciplinas disponíveis para matrícula:\n"
        exibeDisciplinasDisponiveis aluno
        putStrLn "\nInforme a sigla da disciplina na qual deseja se matricular:"
        sigla <- getLine
        siglasCadastradas <- getSiglas
        if sigla `elem` siglasCadastradas then
            if sigla `elem` (disciplinas aluno) then do 
                putStrLn ("Você já está matriculado em " ++ sigla ++ ", tente novamente\n\n")
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
        putStrLn "\nVocê está matriculado nas seguintes disciplinas:\n"
        putStrLn (show(disciplinas aluno))
        putStrLn "Informe a sigla da disciplina na qual deseja se desmatricular: "
        sigla <- getLine 
        if sigla `elem` (disciplinas aluno) then do
            let disciplinasExcetoMencionada = filter (/= sigla) (disciplinas aluno)
            let alunoAtualizado = Aluno (A.id aluno) (nome aluno) (disciplinasExcetoMencionada)
            atualizaLinhaById "Alunos" (show (A.id aluno)) (show alunoAtualizado)
            putStrLn "Cancelamento de matricula realizada com sucesso!\n"
        else do 
            putStrLn ("Insira um valor válido!\n\n")
            desmatriculaAlunoDeDisciplina aluno
    
    -- DEVE SER MOVIDO PARA OUTRO CONTROLADOR
    resolveTicket :: Aluno -> IO()
    resolveTicket aluno = do
        ticketsAluno <- getTicketsAluno (A.id aluno)
        putStrLn "Estes são os seus tickets com status \"Em andamento\"\n"
        exibeTickets ticketsAluno
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

    -- DEVE SER MOVIDO PARA OUTRO CONTROLADOR
    adicionaMensagemAluno :: Aluno -> IO()
    adicionaMensagemAluno aluno = do
        putStrLn "Foram identificados os seguintes tickets desse autor: "
        tickets <- getTicketsAluno (A.id aluno)
        print tickets
        adicionaMensagem (A.id aluno)