module Src.Controller.DisciplinaController where
    import Src.Model.Disciplina as D
    import Src.Util.TxtFunctions
    import Src.Model.Aluno as A
    
    exibeDisciplinas:: Aluno -> IO()
    exibeDisciplinas aluno = do
        disciplinas <- fileToStringArray "Disciplinas"
        exibeDisciplinasRecursivo disciplinas aluno
    
    exibeDisciplinasRecursivo :: [String] -> Aluno  -> IO()
    exibeDisciplinasRecursivo [] _ = return ()
    exibeDisciplinasRecursivo (disciplinaAtual:disciplinasRestantes) aluno = do
        let disciplina = (read disciplinaAtual :: Disciplina)
        if (show (D.sigla disciplina)) `elem` (disciplinas aluno)
            then exibeDisciplinasRecursivo disciplinasRestantes aluno else do
            putStrLn (show (D.id disciplina) ++ ") " ++ show (D.nome disciplina) ++ " - " ++ show (sigla disciplina))
            exibeDisciplinasRecursivo disciplinasRestantes aluno
        