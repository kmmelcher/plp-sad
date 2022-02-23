module Src.Controller where
    import Src.Util.TxtFunctions
    import Src.Model.Aluno

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
