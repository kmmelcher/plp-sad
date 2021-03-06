-- O módulo AlunoController fornece todas as funcionalidades necessárias de interação para lidar com o Tipo
-- Aluno. Funciona tanto como mediador de informações entre um controller e o Aluno, como próprio canal de 
-- troca de informações entre o usuário e o sistema. 

module Controller.AlunoController where
    import Model.Aluno as A
    import Util.TxtFunctions
    import Controller.DisciplinaController as DC
    import Model.Monitor as M
    import Data.Char
    import Util.EncriptFunctions

    --Função que retorna um Aluno com o id fornecido
    --  > Parametros:
    --    id = matricula do aluno desejado
    getAluno:: Int -> IO Aluno
    getAluno id = do
        alunoToString <- getObjetoById "Alunos" id
        return (read alunoToString :: Aluno)


    -- Vincula o aluno a uma disciplina, caso ele já não seja monitor, interagindo com o usuário para obtenção de 
    -- informações de entrada. 
    --  > Parametros:
    --    disciplina = string contendo a sigla da disciplina na qual o aluno será vinculado

    vinculaAluno :: String -> IO()
    vinculaAluno disciplina = do
        putStrLn "Insira a matricula do aluno (digite 0 para voltar ao seu menu)"
        matricula <- readLn

        if matricula == 0 then return () else do
            ehAluno <- checaExistenciaById "Alunos" matricula
            ehMonitorDaDisciplina <- analisaAlunoComoMonitor matricula disciplina
            if ehMonitorDaDisciplina then putStrLn "Este aluno é monitor de sua disciplina!"
            else if ehAluno then do
                    aluno <- getAluno matricula
                    if disciplina `elem` A.disciplinas aluno then putStrLn "Este aluno já está na sua diciplina!"
                    else do
                        let alunoAtualizado = Aluno (A.id aluno) (nome aluno) (disciplina : disciplinas aluno) (A.senha aluno)
                        atualizaLinhaById "Alunos" (show matricula) (show alunoAtualizado)
                        putStrLn "Este aluno já está presente no sistema. Cadastro de aluno na disciplina realizado!"
            else do
                putStrLn "Este aluno não está cadastrado no SAD. Por favor, informe seu nome:"
                nome <- getLine
                let aluno = Aluno matricula nome [disciplina] (encripta "Aluno" nome)
                adicionaLinha "Alunos" $ show aluno
                putStrLn "Aluno cadastrado com sucesso e incluso na disciplina.\n"


    -- Vincula o aluno a uma disciplina, caso ele já não seja monitor, interagindo com o usuário para obtenção de 
    -- informações de entrada. 
    --  > Parametros:
    --    disciplina = disciplina na qual o aluno será vinculado
    desvinculaAluno :: String -> IO()
    desvinculaAluno disciplina = do
        putStrLn "Informe a matrícula do aluno a ser desvinculado de sua disciplina (digite 0 para voltar ao seu menu):"
        matricula <- readLn
        if matricula == 0 then return () else do
            alunoExiste <- checaExistenciaById "Alunos" matricula
            monitorExisteNaDisciplina <- analisaAlunoComoMonitor matricula disciplina
            if monitorExisteNaDisciplina then putStrLn "Este aluno é monitor da sua disciplina!\n"
            else if alunoExiste then do
                aluno <- getAluno matricula
                if disciplina `elem` A.disciplinas aluno then do
                    let alunoAtualizado = Aluno (A.id aluno) (nome aluno) (filter (/= disciplina) (disciplinas aluno)) (A.senha aluno)
                    atualizaLinhaById "Alunos" (show (A.id aluno)) (show alunoAtualizado)
                    putStrLn "O aluno foi desvinculado com sucesso.\n"
                else putStrLn "Este aluno não está matriculado na sua disciplina!\n"
            else putStrLn "Este aluno não está cadastrado no sistema.\n"

    -- Informa se um aluno cursa uma determinada disciplina.
    --  > Parametros:
    --    matricula = a matricula do aluno
    --    disciplina = a sigla da disciplina a ser analisada
    alunoCursaDisciplina :: Int -> String -> IO Bool
    alunoCursaDisciplina matricula disciplina = do
        aluno <- getAluno matricula
        return (disciplina `elem` A.disciplinas aluno)

    -- Verifica se a matrícula de um aluno é referente a um monitor de uma determinada disciplina
    --  > Parametros:
    --    matricula = matricula a ser analisada
    --    disciplina = disciplina a ser verificada sobre a existencia do monitor
    analisaAlunoComoMonitor :: Int -> String -> IO Bool
    analisaAlunoComoMonitor matricula disciplina = do
        instanciaMonitor <- getObjetoById "Monitores" matricula
        if instanciaMonitor /= "" then do
            let monitor = read instanciaMonitor :: M.Monitor
            return (M.disciplina monitor == disciplina)
        else return False
