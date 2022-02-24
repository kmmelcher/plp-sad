module Src.Controller where
    import Src.Util.TxtFunctions
    import Src.Model.Aluno
    import Src.Model.Professor
    import Data.Time (getCurrentTime, UTCTime)
    import Src.Model.Mensagem
    import Data.Time.Format
    
    addProfessor :: IO()
    addProfessor = do
            putStrLn "Insira o nome do professor: "
            nome <- getLine
            putStrLn "Insira o nome das disciplinas do professor: "
            listaDisciplinasStr <- getLine
            let disciplinas = read(listaDisciplinasStr) :: [String]
            id <- buscaNovoId "Professores"
            let prof = Professor (read id :: Int) nome disciplinas
            let profToString = show (prof)
            adicionaLinha "../database/Professores.txt" profToString

    createAluno :: IO()
    createAluno = do
        putStrLn "Insira o nome do aluno: "
        nome <- getLine
        putStrLn "Insira a matricula do aluno"
        matricula <- readLn  
        putStrLn "Insira as disciplinas do aluno"
        disciplinas <- readLn 
        let aluno = Aluno matricula nome disciplinas
        adicionaLinha "alunos" $ show aluno
    
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
    
    main :: IO()
    main = adicionaMensagem