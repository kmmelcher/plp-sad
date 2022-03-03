module Src.Controller.ProfessorController where
    import Src.Util.TxtFunctions
    import Src.Controller.ChatController
    import Src.Model.Ticket
    import Src.Controller.AlunoController
    import qualified Src.Model.Professor as P
    import Control.Exception (evaluate)

    getProfessor:: Int -> IO P.Professor
    getProfessor id = do
        professorToString <- getObjetoById "Professores" id
        return (read professorToString :: P.Professor)

    adicionaProfessor :: IO()
    adicionaProfessor = do
            putStrLn "Insira o nome do professor: "
            nome <- getLine
            putStrLn "Insira o nome das disciplinas do professor: "
            listaDisciplinasStr <- getLine
            let disciplinas = read listaDisciplinasStr :: [String]
            id <- buscaNovoId "Professores"
            let prof = P.Professor (read id :: Int) nome disciplinas
            let profToString = show prof
            adicionaLinha "Professores" profToString
            putStrLn ("Professor cadastrado com sucesso no id " ++ id ++ ". Decore seu id para utilizar o sistema!\n")

    lerTicketsDisciplina :: P.Professor -> IO()
    lerTicketsDisciplina professor = do
        if length (P.disciplinas professor) > 1 then do
            putStrLn "Insira qual disciplina você deseja visualizar os tickets:"
            disciplina <- getLine
            if verificaDisciplina (P.disciplinas professor) disciplina
                then exibeTicketsDisciplina disciplina
                else do
                    putStrLn "\nDisciplina invalida!"
                    lerTicketsDisciplina professor
        else do
               exibeTicketsDisciplina (head (P.disciplinas professor))

    verificaDisciplina :: [String] -> String -> Bool
    verificaDisciplina [] _ = False
    verificaDisciplina (disciplinaAtual:disciplinasRestantes) disciplina = do
        (disciplinaAtual == disciplina) || verificaDisciplina disciplinasRestantes disciplina

    adicionaMensagemProfessor :: P.Professor -> IO ()
    adicionaMensagemProfessor professor = do
        let disciplinasDoProfessor = P.disciplinas professor
        ticketsValidos <- pegaTicketsDoProfessor disciplinasDoProfessor
        adicionaMensagem (P.id professor) ticketsValidos


    pegaTicketsDoProfessor :: [String] -> IO [Int]
    pegaTicketsDoProfessor [] = return []
    pegaTicketsDoProfessor (head : tail) = do
        todosOsTickets <- getTicketsDisciplina head
        ticketsFiltrados <- getTicketsEmAndamento todosOsTickets
        exibeTickets ticketsFiltrados ("para a disciplina " ++ head) ("da disciplina " ++ head)
        result <- pegaTicketsDoProfessor tail
        return (ticketsFiltrados ++ result)




