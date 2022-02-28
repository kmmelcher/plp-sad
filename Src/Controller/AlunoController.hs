module Src.Controller.AlunoController where
    import Src.Model.Aluno as A
    import Src.Util.TxtFunctions
    import Src.Controller.DisciplinaController as DC
    
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
    
    matriculaAlunoEmDisciplina :: Aluno -> IO()
    matriculaAlunoEmDisciplina aluno = do
        putStrLn "Atualmente, estas são todas as disciplinas disponíveis para matrícula, exibidas em id, nome e sigla:\n\n"
        exibeDisciplinas aluno
        putStrLn "\nInforme a sigla da disciplina na qual deseja se matricular:"
        sigla <- getLine 
        -- CHECA SE SIGLA EXISTE
        let alunoAtualizado = Aluno (A.id aluno) (nome aluno) ([sigla] ++ disciplinas aluno)
        atualizaLinhaById "Alunos" (show (A.id aluno)) (show aluno)
        putStrLn "Matricula realizada com sucesso!"