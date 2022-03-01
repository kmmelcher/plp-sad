module Src.Controller.ProfessorController where
    import Src.Model.Professor
    import Src.Util.TxtFunctions
    import Src.Controller.ChatController (pegaTicketsDeUmaDisciplina)
    import Src.Model.Ticket
    import Src.Controller.AlunoController

    adicionaProfessor :: IO()
    adicionaProfessor = do
            putStrLn "Insira o nome do professor: "
            nome <- getLine
            putStrLn "Insira o nome das disciplinas do professor: "
            listaDisciplinasStr <- getLine
            let disciplinas = read listaDisciplinasStr :: [String]
            id <- buscaNovoId "Professores"
            let prof = Professor (read id :: Int) nome disciplinas
            let profToString = show prof
            adicionaLinha "Professores" profToString
            putStrLn ("Professor cadastrado com sucesso no id " ++ id ++ ". Decore seu id para utilizar o sistema!\n")

    lerTicketsDisciplina :: Int -> IO()
    lerTicketsDisciplina idProfessor = do
         profStr <- buscaObjetoById "Professores" idProfessor
         let professor = read profStr :: Professor
         putStrLn "\nDisciplinas cadastradas: "
         printArray (disciplinas professor)
         sel <- getLine
         if verificaDisciplina (disciplinas professor) sel
            then do
                tickets <- pegaTicketsDeUmaDisciplina sel
                if null tickets
                    then putStrLn "Não há tickets para essa disciplina"
                    else mostraTickets tickets
            else
                print "Disciplina invalida!"

    printArray :: [String] -> IO()
    printArray [] = putStr "\nEntre Com a sigla da disciplina: "
    printArray (head:tail) = do
        print head
        printArray tail

    verificaDisciplina :: [String] -> String -> Bool
    verificaDisciplina [] x = False
    verificaDisciplina (head:tail) x = do
        (head == x) || verificaDisciplina tail x

