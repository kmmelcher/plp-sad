module Src.Controller where
    import Src.Util.TxtFunctions
    import Src.Model.Aluno
    import Src.Model.Professor
    import Src.Model.Ticket
    import Control.Monad (when)
    import Data.Char (GeneralCategory(Control))
    import qualified Src.Model.Ticket as Src.Model
    
    adicionaProfessor :: IO()
    adicionaProfessor = do
            putStrLn "Insira o nome do professor: "
            nome <- getLine
            putStrLn "Insira o nome das disciplinas do professor: "
            listaDisciplinasStr <- getLine
            let disciplinas = read(listaDisciplinasStr) :: [String]
            id <- buscaNovoId "Professores"
            let prof = Professor id nome disciplinas
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


