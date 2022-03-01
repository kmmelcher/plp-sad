module Src.Controller.ChatController where
    import Src.Model.Mensagem
    import qualified Src.Model.Ticket as T
    import Control.Monad (when)
    import Src.Util.TxtFunctions
    import Data.Time (getCurrentTime, UTCTime)
    import Data.Time.Format
    import qualified Src.Model.Ticket as T
    
    adicionaTicket :: IO()
    adicionaTicket = do
        putStrLn "\nInsira o id do solicitante: "
        autor <- getLine
        putStrLn "Insira o nome da disciplina que você tem dúvida:"
        disciplinaTicket <-  getLine
        putStrLn "Insira um título para sua dúvida:"
        titulo <- getLine
        id <- buscaNovoId "Tickets"
        let ticket = T.Ticket (read id) (titulo) [] "Em Andamento" (read autor) disciplinaTicket
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
        idTicket <- readLn
        putStrLn "Digite a mensagem:"
        conteudo <- getLine
        idMensagem <- buscaNovoId "Mensagens"
        tempo <- getCurrentTime >>= return.(formatTime defaultTimeLocale "%D %Hh%M")
        let mensagem = Mensagem (read idMensagem) autor conteudo tempo
        inserirMensagemNoTicket tickets idTicket autor (read idMensagem)
        adicionaLinha "Mensagens" $ show (mensagem)
    
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
    
    -- Parametros
    -- ticketsAutor : tickets aberto pelo autor, lembrando que somente alunos podem abrir ticket. 
    -- idTicket : ticket no qual voce quer inserir a mensagem. Caso vc seja aluno, o id deve se encontrar entre os valores
    -- descritos em ticketsAutor. Caso vc seja monitor ou professor não tem restrições desde que vc esteja vinculado a disciplina.
    -- idAutor : matricula no caso de aluno, ou então idProfessor
    -- idMensagem
    inserirMensagemNoTicket :: [Int] -> Int -> Int -> Int -> IO()
    inserirMensagemNoTicket ticketsAutor idTicket idAutor idMensagem = do 
        ticketStr <- buscaObjetoById "Tickets" idTicket
        let ticket = read (ticketStr) :: T.Ticket
        let ticketAtualizado = T.Ticket idTicket (T.titulo(ticket)) (idMensagem:(T.mensagens(ticket))) (T.status(ticket))(T.autor(ticket)) (T.disciplina(ticket))
        atualizaLinhaById "Tickets" (show idTicket) (show ticketAtualizado)

    pegaTicketsDeUmaDisciplina :: String -> IO[Int]
    pegaTicketsDeUmaDisciplina disciplina = do 
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
    
    exibeTicketsEmAndamento :: [Int] -> IO()
    exibeTicketsEmAndamento [] = return ()
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
        return ((T.status ticket) == "Em Andamento")
    
    marcaTicketComoConcluido :: Int -> IO()
    marcaTicketComoConcluido id = do
        instanciaTicket <- buscaObjetoById "Tickets" id
        let ticket = read instanciaTicket :: T.Ticket
        let novoTicket = T.Ticket (T.id ticket) (T.titulo ticket) (T.mensagens ticket) ("Resolvido") (T.autor ticket) (T.disciplina ticket)
        atualizaLinhaById "Tickets" (show id) (show novoTicket)
