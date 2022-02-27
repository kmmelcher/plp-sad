module Src.Controller.AlunoController where
    import Src.Model.Aluno
    import Src.Util.TxtFunctions
    
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
        putStrLn "Aluno cadastrado com sucesso.\n"