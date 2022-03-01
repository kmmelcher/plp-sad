module Src.Controller.MonitorController where
    import Src.Model.Monitor
    import Src.Util.TxtFunctions

    adicionaMonitor :: IO()
    adicionaMonitor = do
        id <- insereMatricula
        if id == 0
            then putStrLn "Cadastro cancelado.\n"
        else do 
            disciplina <- insereDisciplina
            if disciplina == "VOLTAR"
                then putStrLn "Cadastro cancelado.\n"
            else do
                putStrLn "Insira os horários de atendimento do monitor"
                horarios <- getLine
                let monitor = Monitor id disciplina horarios
                adicionaLinha "Monitores" $ show monitor
                putStrLn "Monitor cadastrado com sucesso!\n"

    insereMatricula :: IO(Int)
    insereMatricula = do
        putStrLn "Insira a matricula do monitor"
        id <- readLn
        alunoCadastrado <- checaExistenciaById "Alunos" id
        if alunoCadastrado || id == 0
            then return id
        else do
            putStrLn "Aluno não cadastrado!\n(Digite 0 para voltar ao menu principal)\n"
            insereMatricula

    insereDisciplina :: IO(String)
    insereDisciplina = do
        putStrLn "Insira a disciplina do monitor"
        disciplina <- getLine
        disciplinaCadastrada <- checaExistenciaByAtributo  "Disciplinas" "sigla" (adicionaAspas disciplina)
        if disciplinaCadastrada || disciplina == "VOLTAR"
            then return disciplina
        else do
            putStrLn "Disciplina não cadastrada!\n(Digite \"VOLTAR\" para voltar ao menu principal)\n"
            insereDisciplina
