module Src.Controller.ChatController where
    import Src.Model.Mensagem
    import Src.Model.Aluno
    import qualified Src.Model.Ticket as T
    import Control.Monad (when)
    import Src.Util.TxtFunctions
    import Data.Time (getCurrentTime, UTCTime)
    import Data.Time.Format

    adicionaTicket :: Aluno -> IO()
    adicionaTicket aluno = do
        putStrLn "Insira o nome da disciplina que você tem dúvida:"
        disciplinaTicket <-  getLine
        if disciplinaTicket `elem` (disciplinas aluno) then do
            putStrLn "Insira um título para sua dúvida:"
            titulo <- getLine
            id <- buscaNovoId "Tickets"
            let ticket = T.Ticket (read id) (titulo) [] "Em Andamento" () disciplinaTicket
            adicionaLinha "Tickets" $ show ticket
            putStrLn "Deseja adicionar mais um ticket? (s/n)"
            resposta <- getLine
            Control.Monad.when (resposta == "s") $ do
                    adicionaTicket
        else do 
            putStrLn ("Você não está matriculado na disciplina " ++ disciplinaTicket) 
    
    adicionaMensagem :: Int -> IO()
    adicionaMensagem id = do
        putStrLn "Escolha o ticket no qual deseja inserir a mensagem: "
        idTicket <- readLn
        putStrLn "Digite a mensagem:"
        conteudo <- getLine
        idMensagem <- buscaNovoId "Mensagens"
        tempo <- getCurrentTime >>= return.(formatTime defaultTimeLocale "%D %Hh%M")
        let mensagem = Mensagem (read idMensagem) id conteudo tempo
        inserirMensagemNoTicket idTicket (read idMensagem)
        adicionaLinha "Mensagens" $ show mensagem
    
    pegaTicketsDoAluno :: Int -> IO[Int]
    pegaTicketsDoAluno matricula = do
        tickets <- fileToStringArray "Tickets"
        return(comparaAutorDeTodosTickets tickets matricula) 
        
    comparaAutorDeUmTicket :: String -> Int -> Bool
    comparaAutorDeUmTicket str i = do 
        let ticket = read(str) :: T.Ticket
        if T.autor(ticket) == i then True else False
    
    comparaAutorDeTodosTickets :: [String] -> Int -> [Int]
    comparaAutorDeTodosTickets [] i = []
    comparaAutorDeTodosTickets (x:xs) i = if ( comparaAutorDeUmTicket x i ) 
        then retornaIdTicket x : (comparaAutorDeTodosTickets xs i) 
        else [] ++ (comparaAutorDeTodosTickets xs i)

    retornaIdTicket :: String -> Int 
    retornaIdTicket t = do 
        let ticket = read(t) :: T.Ticket 
        T.id(ticket)
    
    {- 
    Insere uma mensagem em um Ticket.
    Parametros
        idTicket : ticket no qual voce quer inserir a mensagem.
        idMensagem: Mensagem a ser inserida no Ticket.
    -}
    inserirMensagemNoTicket :: Int -> Int -> IO()
    inserirMensagemNoTicket idTicket idMensagem = do 
        ticketStr <- buscaObjetoById "Tickets" idTicket
        let ticket = read ticketStr :: T.Ticket
        let ticketAtualizado = T.Ticket idTicket (T.titulo ticket) (idMensagem: T.mensagens ticket) (T.status ticket) (T.autor ticket) (T.disciplina ticket)
        atualizaLinhaById "Tickets" (show idTicket) (show ticketAtualizado)


    {- 
    Retorna todos os tickets de uma disciplina
    Parametros
        disciplina: qual o nome da disciplina dos tickets que queremos retornar
    -}
    getTicketsDisciplina :: String -> IO[Int]
    getTicketsDisciplina disciplina = do 
        tickets <- fileToStringArray "Tickets"
        return(comparaDisciplinaDeTodosTickets tickets disciplina) 
    
    comparaDisciplinaDeUmTicket :: String -> String -> Bool
    comparaDisciplinaDeUmTicket str disciplina = do 
        let ticket = read(str) :: T.Ticket
        if T.disciplina(ticket) == disciplina then True else False
    
    comparaDisciplinaDeTodosTickets :: [String] -> String -> [Int]
    comparaDisciplinaDeTodosTickets [] disciplina = []
    comparaDisciplinaDeTodosTickets (x:xs) disciplina = if ( comparaDisciplinaDeUmTicket x disciplina ) 
        then retornaIdTicket x : (comparaDisciplinaDeTodosTickets xs disciplina) 
        else [] ++ (comparaDisciplinaDeTodosTickets xs disciplina)
    

    {- 
    Exibe todos os tickets de uma disciplina.
    Parâmetros:
        nomeDisciplina: Nome da disciplina a ser exibida.
    -}
    exibeTicketsDisciplina :: String -> IO()
    exibeTicketsDisciplina nomeDisciplina = do
        tickets <- getTicketsDisciplina nomeDisciplina
        if null tickets 
            then putStrLn "\nAinda não há tickets nesta disciplina.\n" 
        else do
            putStrLn ("\nEstes são os tickets existentes da disciplina: " ++ nomeDisciplina)
            exibeTicketsDisciplinaRecursivo tickets
    
    {-
    Itera recursivamente sobre todos os tickets da disciplina exibindo cada um.
    -}
    exibeTicketsDisciplinaRecursivo :: [Int] -> IO()
    exibeTicketsDisciplinaRecursivo [] = do
        putStr "\n"
        return ()
    exibeTicketsDisciplinaRecursivo (idTicketAtual:idsTicketsRestantes) = do
        instanciaTicket <- buscaObjetoById "Tickets" idTicketAtual
        let ticket = read instanciaTicket :: T.Ticket
        putStrLn (show (T.id ticket) ++ ") " ++ T.titulo ticket ++ " (" ++ T.status ticket ++ ")")
        exibeTicketsDisciplinaRecursivo idsTicketsRestantes

    exibeTicketsEmAndamento :: [Int] -> IO()
    exibeTicketsEmAndamento [] = do
        putStr "\n"
        return ()
    exibeTicketsEmAndamento (ticketAtual:ticketsRestantes) = do
        instanciaTicket <- buscaObjetoById "Tickets" ticketAtual
        let ticket = read instanciaTicket :: T.Ticket
        if T.status ticket == "Em Andamento" then do 
            putStrLn (show (T.id ticket) ++ ") " ++ T.titulo ticket ++ " - " ++ T.disciplina ticket)
            exibeTicketsEmAndamento ticketsRestantes
        else exibeTicketsEmAndamento ticketsRestantes
    
    checaIdDeTicketEmAndamento :: Int -> IO(Bool)
    checaIdDeTicketEmAndamento id = do
        instanciaTicket <- buscaObjetoById "Tickets" id
        let ticket = read instanciaTicket :: T.Ticket
        return (T.status ticket == "Em Andamento")
    
    marcaTicketComoConcluido :: Int -> IO()
    marcaTicketComoConcluido id = do
        instanciaTicket <- buscaObjetoById "Tickets" id
        let ticket = read instanciaTicket :: T.Ticket
        let novoTicket = T.Ticket (T.id ticket) (T.titulo ticket) (T.mensagens ticket) "Resolvido" (T.autor ticket) (T.disciplina ticket)
        atualizaLinhaById "Tickets" (show id) (show novoTicket)
    
    getTicketsEmAndamento :: String -> IO[Int]
    getTicketsEmAndamento disciplina = do
        ticketsDisciplina <- getTicketsDisciplina disciplina
        getTicketsEmAndamentoRecursivo ticketsDisciplina

    getTicketsEmAndamentoRecursivo :: [Int] -> IO([Int])
    getTicketsEmAndamentoRecursivo [] = return ([])
    getTicketsEmAndamentoRecursivo (ticketAtual:ticketsRestantes) = do
        ticketToString <- buscaObjetoById "Tickets" ticketAtual
        let ticket = read ticketToString :: T.Ticket
        if (T.status ticket == "Em Andamento") then do
            proximosTickets <- getTicketsEmAndamentoRecursivo ticketsRestantes
            return ([T.id ticket] ++ proximosTickets)
            else
                getTicketsEmAndamentoRecursivo ticketsRestantes