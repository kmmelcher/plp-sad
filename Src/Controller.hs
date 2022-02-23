module Src.Controller where
    import Src.Util.TxtFunctions
    import Src.Model.Aluno
    import Src.Model.Professor

    addProfessor :: IO()
    addProfessor = do
            putStrLn "Insira o nome do professor: "
            nome <- getLine
            putStrLn "Insira o nome das disciplinas do professor: "
            listaDisciplinasStr <- getLine
            let disciplinas = read(listaDisciplinasStr) :: [String]
            id <- buscaNovoId "Professores"
            let prof = Professor id nome disciplinas
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
