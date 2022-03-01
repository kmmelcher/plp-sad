module Src.Controller.MonitorController where
    import Src.Model.Monitor
    import Src.Util.TxtFunctions

    adicionaMonitor :: IO()
    adicionaMonitor = do
        putStrLn "Insira a matricula do monitor"
        id <- readLn
        alunoCadastrado <- checaExistenciaById "Alunos" id
        if alunoCadastrado then do
            putStrLn "Insira a disciplina do monitor"
            disciplina <- getLine
            disciplinaCadastrada <- checaExistenciaByAtributo  "Disciplinas" "sigla" (adicionaAspas disciplina)
            if disciplinaCadastrada
                then do 
                    putStrLn "Insira os horários de atendimento do monitor"
                    horarios <- getLine
                    let monitor = Monitor id disciplina horarios
                    adicionaLinha "Monitores" $ show monitor
            else do
                putStrLn "Disciplina não cadastrada!\n"
                adicionaMonitor
        else do
            putStrLn "Aluno não cadastrado!\n"
            adicionaMonitor
