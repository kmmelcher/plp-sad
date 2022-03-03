module Src.Controller.ChatController where
    import Src.Model.Mensagem
    import Src.Model.Aluno as A
    import qualified Src.Model.Ticket as T
    import Src.Util.TxtFunctions
    import Data.Time (getCurrentTime, UTCTime)
    import Data.Time.Format
    import Src.Model.Monitor as M
    import Src.Model.Professor as P
    import Src.Controller.AlunoController
    
    getTicket:: Int -> IO T.Ticket
    getTicket id = do
        ticketToString <- getObjetoById "Tickets" id
        return (read ticketToString :: T.Ticket)

    getMensagem:: Int -> IO Mensagem
    getMensagem id = do
        mensagemToString <- getObjetoById "Mensagens" id
        return (read mensagemToString :: Mensagem)

    adicionaTicket :: Aluno -> IO()
    adicionaTicket aluno = do
        putStrLn "\nInsira a sigla da disciplina na qual deseja se desmatricular:"
        disciplinaTicket <-  getLine
        if disciplinaTicket `elem` (A.disciplinas aluno) then do
            putStrLn "Insira um título para sua dúvida:"
            titulo <- getLine
            id <- buscaNovoId "Tickets"
            let ticket = T.Ticket (read id) titulo [] "Em Andamento" (A.id aluno) disciplinaTicket
            adicionaLinha "Tickets" $ show ticket
            putStrLn "\nTicket adicionado com sucesso!"
        else do
            putStrLn ("Você não possui está cadastrado nesta disciplina! Tente novamente.")
            adicionaTicket aluno

    adicionaMensagem :: Int -> [Int] -> IO()
    adicionaMensagem id ticketsValidos = do
        putStrLn "Escolha o ticket no qual deseja inserir a mensagem: "
        idTicket <- readLn
        if ehTicketValido idTicket ticketsValidos
            then do
            putStrLn "Digite a mensagem:"
            conteudo <- getLine
            idMensagem <- buscaNovoId "Mensagens"
            tempo <- getCurrentTime >>= return.formatTime defaultTimeLocale "%D %Hh%M"
            let mensagem = Mensagem (read idMensagem) id conteudo tempo
            insereMensagemNoTicket idTicket (read idMensagem)
            adicionaLinha "Mensagens" $ show mensagem
            putStrLn "Mensagem adicionada com sucesso."
        else do
            putStrLn "Ticket inválido!\n"
            adicionaMensagem id ticketsValidos

    ehTicketValido :: Int -> [Int] -> Bool
    ehTicketValido ticketId ticketsValidos = ticketId `elem` ticketsValidos

    -- Não poderia retornar um array de tickets?
    getTicketsAluno :: Int -> IO[Int]
    getTicketsAluno matricula = do
        tickets <- fileToStringArray "Tickets"
        getTicketsAlunoRecursivo tickets matricula

    getTicketsAlunoRecursivo :: [String] -> Int -> IO([Int])
    getTicketsAlunoRecursivo [] _ = return ([])
    getTicketsAlunoRecursivo (ticketAtual:ticketsRestantes) matricula = do
        let ticket = (read ticketAtual :: T.Ticket)
        proximos <- getTicketsAlunoRecursivo ticketsRestantes matricula
        if T.autor ticket == matricula then do
            return (T.id ticket : proximos)
            else return proximos

    {- 
    Insere uma mensagem em um Ticket.
    Parametros
        idTicket : ticket no qual voce quer inserir a mensagem.
        idMensagem: Mensagem a ser inserida no Ticket.
    -}
    insereMensagemNoTicket :: Int -> Int -> IO()
    insereMensagemNoTicket idTicket idMensagem = do
        ticket <- getTicket idTicket
        let ticketAtualizado = T.Ticket idTicket (T.titulo ticket) (T.mensagens ticket ++ [idMensagem]) (T.status ticket) (T.autor ticket) (T.disciplina ticket)
        atualizaLinhaById "Tickets" (show idTicket) (show ticketAtualizado)

    {-
    Exibe todos os tickets de uma disciplina.
    Parâmetros:
        nomeDisciplina: Nome da disciplina a ser exibida.
    -}
    exibeTicketsDisciplina :: String -> IO()
    exibeTicketsDisciplina nomeDisciplina = do
        tickets <- getTicketsDisciplina nomeDisciplina
        exibeTickets tickets "nesta disciplina" ("da disciplina: " ++ nomeDisciplina)

    {- 
    Retorna todos os tickets de uma disciplina
    Parametros
        disciplina: qual o nome da disciplina dos tickets que queremos retornar
    -}
    getTicketsDisciplina :: String -> IO[Int]
    getTicketsDisciplina disciplina = do
        tickets <- fileToStringArray "Tickets"
        getTicketsDisciplinaRecursivo tickets disciplina
    
    getTicketsDisciplinaRecursivo :: [String] -> String -> IO[Int]
    getTicketsDisciplinaRecursivo [] _ = return ([])
    getTicketsDisciplinaRecursivo (ticketAtual:ticketsRestantes) disciplina = do
        let ticket = (read ticketAtual :: T.Ticket)
        if (T.disciplina ticket) == disciplina
            then do
                proximos <- getTicketsDisciplinaRecursivo ticketsRestantes disciplina
                return ([T.id ticket] ++ proximos)
            else getTicketsDisciplinaRecursivo ticketsRestantes disciplina
    
    exibeTicketsEmAndamento :: [Int] -> IO()
    exibeTicketsEmAndamento tickets = do
        ticketsEmAndamento <- getTicketsEmAndamento tickets
        exibeTickets ticketsEmAndamento "em andamento criados por você" "e em andamento criados por você:"

    getTicketsEmAndamento :: [Int] -> IO[Int]
    getTicketsEmAndamento tickets = do
        getTicketsEmAndamentoRecursivo tickets

    getTicketsEmAndamentoRecursivo :: [Int] -> IO [Int]
    getTicketsEmAndamentoRecursivo [] = return []
    getTicketsEmAndamentoRecursivo (ticketAtual:ticketsRestantes) = do
        ticket <- getTicket ticketAtual
        if T.status ticket == "Em Andamento" then do
            proximosTickets <- getTicketsEmAndamentoRecursivo ticketsRestantes
            return (T.id ticket : proximosTickets)
            else
                getTicketsEmAndamentoRecursivo ticketsRestantes

    
    getTicketsConcluidos :: [Int] -> IO[Int]
    getTicketsConcluidos tickets = do
        getTicketsConcluidosRecursivo tickets

    getTicketsConcluidosRecursivo :: [Int] -> IO [Int]
    getTicketsConcluidosRecursivo [] = return []
    getTicketsConcluidosRecursivo (ticketAtual:ticketsRestantes) = do
        ticket <- getTicket ticketAtual
        if T.status ticket == "Resolvido" then do
            proximosTickets <- getTicketsConcluidosRecursivo ticketsRestantes
            return (T.id ticket : proximosTickets)
            else
                getTicketsConcluidosRecursivo ticketsRestantes




    {-
    Exibe uma lista de tickets.
    Checa se a lista de tickets é vazia, nesse caso é avisado ao usuário.
    -}
    exibeTickets :: [Int] -> String -> String -> IO()
    exibeTickets tickets mensagemSemTicket mensagemComTickets = do
        if null tickets
            then putStrLn ("\nAinda não há tickets " ++ mensagemSemTicket ++ ".\n")
        else do
            putStrLn ("\nEstes são os tickets existentes " ++ mensagemComTickets ++ "\n")
            exibeTicketsRecursivo tickets

    {-
    Itera recursivamente sobre uma lista de tickets exibindo cada um.
    -}
    exibeTicketsRecursivo :: [Int] -> IO()
    exibeTicketsRecursivo [] = do
        putStr "\n"
        return ()
    exibeTicketsRecursivo (idTicketAtual:idsTicketsRestantes) = do
        ticket <- getTicket idTicketAtual
        putStrLn (show (T.id ticket) ++ ") " ++ T.titulo ticket ++ " (" ++ T.status ticket ++ ")")
        exibeTicketsRecursivo idsTicketsRestantes

    checaIdDeTicketEmAndamento :: Int -> IO Bool
    checaIdDeTicketEmAndamento id = do
        ticket <- getTicket id
        return (T.status ticket == "Em Andamento")

    atualizaTicketStatus :: Int -> IO()
    atualizaTicketStatus id = do
        ticket <- getTicket id
        let novoTicket = T.Ticket (T.id ticket) (T.titulo ticket) (T.mensagens ticket) "Resolvido" (T.autor ticket) (T.disciplina ticket)
        atualizaLinhaById "Tickets" (show id) (show novoTicket)

    resolveTicket :: Aluno -> IO()
    resolveTicket aluno = do
        ticketsAluno <- getTicketsAluno (A.id aluno)
        putStrLn "Estes são os seus tickets com status \"Em andamento\"\n"
        exibeTicketsEmAndamento ticketsAluno
        putStrLn "\ninsira o id do ticket que deseja marcar como concluído"
        input <- getLine
        let id = read input :: Int
        idValido <- checaIdDeTicketEmAndamento id
        if idValido then do
            atualizaTicketStatus id
            putStrLn "Ticket alterado com sucesso.\n"
             else do
            putStrLn "Insira um id válido!"
            resolveTicket aluno

    adicionaMensagemMonitor :: Monitor -> IO()
    adicionaMensagemMonitor monitor = do
        tickets <- getTicketsDisciplina (M.disciplina monitor)
        exibeTicketsEmAndamento tickets
        ticketsEmAndamento <- getTicketsEmAndamento tickets
        if null tickets
            then return ()
        else do
            adicionaMensagem (M.id monitor) ticketsEmAndamento

    adicionaMensagemAluno :: Aluno -> IO()
    adicionaMensagemAluno aluno = do
        tickets <- getTicketsAluno (A.id aluno)
        ticketsEmAndamento <- getTicketsEmAndamento tickets
        if null ticketsEmAndamento then exibeTicketsEmAndamento tickets else do
            exibeTicketsEmAndamento tickets
            adicionaMensagem (A.id aluno) ticketsEmAndamento

    excluirTicket :: Int -> IO()
    excluirTicket matAluno = do
        ticketsIds <- getTicketsAluno matAluno
        exibeTickets ticketsIds "para exclusão" "existentes"
        putStrLn "Escolha entre os seus Tickets qual será excluido: "
        sel <- getLine
        if verificaTicket ticketsIds (read sel)
            then removeLinha "Tickets" sel
            else print "Ticket invalido"

    verificaTicket :: [Int] -> Int -> Bool
    verificaTicket [] x = False
    verificaTicket (head:tail) x = do
        (head == x) || verificaTicket tail x

    leTicketsDoAluno :: Aluno -> IO()
    leTicketsDoAluno aluno = do
        ticketsAluno <- (getTicketsAluno (A.id aluno))
        if null ticketsAluno then exibeTickets ticketsAluno "de sua autoria" "criados por você" else do
            exibeTickets ticketsAluno "de sua autoria" "criados por você"
            putStrLn "Insira o id do ticket que deseja ler as mensagens: "
            idTicket <- readLn
            if idTicket `elem` ticketsAluno then exibeMensagensDeTicket idTicket else do 
                putStrLn "Id de ticket invalido!"
                leTicketsDoAluno aluno

    exibeMensagensDeTicket :: Int -> IO()
    exibeMensagensDeTicket idTicket = do 
        mensagens <- getMensagensDoTicket idTicket 
        exibeMensagensDoTicketRecursivo mensagens
    
    exibeMensagensDoTicketRecursivo :: [IO Mensagem] -> IO()
    exibeMensagensDoTicketRecursivo [] = return ()
    exibeMensagensDoTicketRecursivo (mensagemAtualIO:mensagensRestantesIO) = do
        mensagemAtual <- mensagemAtualIO
        exibeMensagem mensagemAtual
        exibeMensagensDoTicketRecursivo mensagensRestantesIO
    
    getMensagensDoTicket :: Int -> IO[IO Mensagem]
    getMensagensDoTicket idTicket = do 
        ticket <- getTicket idTicket
        let mensagensTicket = T.mensagens ticket
        return(map getMensagem mensagensTicket)

    exibeMensagem :: Mensagem -> IO() 
    exibeMensagem mensagem = do
        ehProf <- checaExistenciaById "Professores" (autor mensagem)
        ehMonitor <- checaExistenciaById "Monitores" (autor mensagem)
        ehAluno <- checaExistenciaById "Alunos" (autor mensagem)
        if ehProf then do 
            professor <- pegaProfessor (autor mensagem)
            putStrLn ("[" ++ (horario mensagem) ++ "] " ++ "(PROFESSOR) " ++ (P.nome professor) ++ " - " ++ (conteudo mensagem)) 
        else do 
            aluno <- getAluno (autor mensagem)
            if ehMonitor then
                putStrLn ("[" ++ (horario mensagem) ++ "] " ++ "(MONITOR) " ++ (A.nome aluno) ++ " - " ++ (conteudo mensagem)) 
            else putStrLn ("[" ++ (horario mensagem) ++ "] " ++ "(ALUNO) " ++ (A.nome aluno) ++ " - " ++ (conteudo mensagem)) 
    
    -- Peguei de forma temporaria do ProfessorController para nao precisar importar (tá dando import ciclico) 
    pegaProfessor:: Int -> IO Professor
    pegaProfessor id = do
        professorToString <- getObjetoById "Professores" id
        return (read professorToString :: Professor)
    
    leTicketsDaDisciplinaAluno :: Aluno -> IO()
    leTicketsDaDisciplinaAluno aluno = do
        putStrLn "\nInforme a sigla da disciplina na qual deseja visualizar os tickets:"
        disciplina <- getLine
        if disciplina `elem` (A.disciplinas aluno) then do
            exibeMensagensDisciplina disciplina
        else do
            putStrLn "\nEsta sigla não é valida ou é referente a uma disciplina na qual você não está matrículado!\n"
            leTicketsDaDisciplinaAluno aluno
    
    exibeMensagensDisciplina :: String -> IO()
    exibeMensagensDisciplina disciplina = do
        tickets <- getTicketsDisciplina disciplina
        if null tickets then exibeTicketsDisciplina disciplina else do
            exibeTicketsDisciplina disciplina
            putStrLn "Deseja visualizar as mensagens de qual ticket?"
            idTicket <- readLn
            ticketsDisciplina <- getTicketsDisciplina disciplina
            if idTicket `elem` ticketsDisciplina then exibeMensagensDeTicket idTicket else do
                putStrLn "Insira um valor válido!"
                exibeMensagensDisciplina disciplina