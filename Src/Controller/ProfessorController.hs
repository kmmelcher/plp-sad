module Src.Controller.ProfessorController where
    import Src.Model.Professor
    import Src.Util.TxtFunctions

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
            putStrLn ("Professor cadastrado com sucesso no id " ++ id ++ ". Decore seu id para utilizar o sistema!\n")