module Src.Controller.ChatController where
    import Src.Model.Mensagem
    import Src.Model.Ticket
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
        let ticket = Ticket (read id) [] "Em progresso" (read autor) disciplinaTicket
        adicionaLinha "Tickets" $ show ticket

        putStrLn "Deseja adicionar mais um ticket? (s/n)"
        resposta <- getLine
        Control.Monad.when (resposta == "s") $ do
                adicionaTicket
    
    adicionaMensagem :: IO()
    adicionaMensagem = do
        putStrLn "Informe o id do ticket referente a essa mensagem"
        ticketId <- getLine

        putStrLn "Insira o id do autor da mensagem: "
        autor <- getLine
        putStrLn "Digite a mensagem:"
        conteudo <- getLine
        idMensagem <- buscaNovoId "Mensagens"
        tempo <- getCurrentTime >>= return.(formatTime defaultTimeLocale "%D %Hh%M")
        let mensagem = Mensagem (read idMensagem) (read autor) conteudo tempo
        -- TODO: INCLUIR MENSAGEM NO ARRAY DE TICKET
        adicionaLinha "mensagens" $ show (mensagem)