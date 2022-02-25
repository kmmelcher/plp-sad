module Src.Controller.ChatController where
    import Src.Model.Mensagem
    import qualified Src.Model.Ticket as Ticket 
    import Control.Monad (when)
    import Src.Util.TxtFunctions
    import Data.Time (getCurrentTime, UTCTime)
    import Data.Time.Format

    adicionaTicket :: IO()
    adicionaTicket = do
        putStrLn "\nInsira o id do solicitante: "
        autor <- getLine
        putStrLn "Insira o nome da disciplina que você tem dúvida:"
        disciplinaTicket <-  getLine

        id <- buscaNovoId "Tickets"
        let ticket = Ticket.Ticket (read id) [] "Em progresso" (read autor) disciplinaTicket
        adicionaLinha "Tickets" $ show ticket

        putStrLn "Deseja adicionar mais um ticket? (s/n)"
        resposta <- getLine
        Control.Monad.when (resposta == "s") $ do
                adicionaTicket
    
    adicionaMensagem :: IO()
    adicionaMensagem = do
        putStrLn "Insira o id do autor da mensagem: "
        autor <- readLn
        putStrLn "Foram identificados os seguintes tickets desse autor: "
        tickets <- pegaTicketsDoAluno autor
        print(tickets)
        putStrLn "Escolha o ticket no qual deseja inserir a mensagem: "
        idTicket <- getLine
        putStrLn "Digite a mensagem:"
        conteudo <- getLine
        idMensagem <- buscaNovoId "Mensagens"
        tempo <- getCurrentTime >>= return.(formatTime defaultTimeLocale "%D %Hh%M")
        let mensagem = Mensagem (read idMensagem) autor conteudo tempo
        --TODO: Falta Repercutir a inclusao da mensagem dentro do ticket
        adicionaLinha "Mensagens" $ show (mensagem)
    
    pegaTicketsDoAluno :: Int -> IO[Int]
    pegaTicketsDoAluno matricula = do
        tickets <- fileToStringArray "./database/Tickets.txt"
        return(comparaTodosTickets tickets matricula) 
        
    comparaUmTicket :: String -> Int -> Bool
    comparaUmTicket str i = do 
        let ticket = read(str) :: Ticket.Ticket
        if Ticket.autor(ticket) == i then True else False
    
    comparaTodosTickets :: [String] -> Int -> [Int]
    comparaTodosTickets [] i = []
    comparaTodosTickets (x:xs) i = if ( comparaUmTicket x i ) 
        then retornaIdTicket x : (comparaTodosTickets xs i) 
        else [] ++ (comparaTodosTickets xs i)

    retornaIdTicket :: String -> Int 
    retornaIdTicket t = do 
        let ticket = read(t) :: Ticket.Ticket 
        Ticket.id(ticket)
    
    --inserirMensagemNoTicket :: [Int] -> Int -> Int -> IO()
    
