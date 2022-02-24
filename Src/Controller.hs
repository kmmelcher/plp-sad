module Src.Controller where
    import Src.Util.TxtFunctions
    import Src.Model.Aluno
    import Src.Model.Professor
    import Data.Time (getCurrentTime, UTCTime)
    import Src.Model.Mensagem
    import Data.Time.Format
    import Src.Model.Ticket
    import Control.Monad (when)
    import Data.Char (GeneralCategory(Control))
    
    adicionaProfessor :: IO()
    adicionaProfessor = do
            putStrLn "Insira o nome do professor: "
            nome <- getLine
            putStrLn "Insira o nome das disciplinas do professor: "
            listaDisciplinasStr <- getLine
            let disciplinas = read(listaDisciplinasStr) :: [String]
            id <- buscaNovoId "Professores"
            let prof = Professor (read id :: Int) nome disciplinas
            let profToString = show (prof)
            adicionaLinha "Professores" profToString

    adicionaAluno :: IO()
    adicionaAluno = do
        putStrLn "Insira o nome do aluno: "
        nome <- getLine
        putStrLn "Insira a matricula do aluno"
        matricula <- readLn
        putStrLn "Insira as disciplinas do aluno"
        disciplinas <- readLn
        let aluno = Aluno matricula nome disciplinas
        adicionaLinha "alunos" $ show aluno

    adicionaTicket :: IO()
    adicionaTicket = do
        putStrLn "\nInsira o nome do solicitante: "
        autor <- getLine
        putStrLn "Insira o nome da disciplina que você tem dúvida:"
        disciplinaTicket <-  getLine

        id <- buscaNovoId "Tickets"
        let intId = read id :: Integer
        let ticket = Ticket intId [] "Em progresso" autor disciplinaTicket
        adicionaLinha "Tickets" $ show ticket

        putStrLn "Deseja adicionar mais um ticket? (y/n)"
        resposta <- getLine
        Control.Monad.when (resposta == "y") $ do
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
        tempo <- getCurrentTime >>= return.(formatTime defaultTimeLocale "%D %Hh%M") >>= putStrLn.show
        let mensagem = Mensagem (read idMensagem :: Int) autor conteudo (show tempo) (read ticketId)
        adicionaLinha "mensagens" $ show (mensagem)
