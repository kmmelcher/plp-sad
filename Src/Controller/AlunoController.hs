module Src.Controller.AlunoController where
    import Src.Model.Aluno as A
    import Src.Util.TxtFunctions
    import Src.Controller.DisciplinaController as DC
    import Src.Model.Monitor as M
    

    {- 
    Função que retorna um objeto do tipo Aluno com o id fornecido
    Parametros:
        id = O id do aluno desejado
    -}
    getAluno:: Int -> IO Aluno
    getAluno id = do
        alunoToString <- getObjetoById "Alunos" id
        return (read alunoToString :: Aluno)

    {- 
    Fução que contém as interações com o usuário nescessárias para criação de um novo aluno
    Forma de uso:
        Função deve ser chamada e os atributos serão inseridos pelo usuário
    -}
    adicionaAluno :: String -> IO()
    adicionaAluno disciplina = do
        putStrLn "Insira a matricula do aluno (digite 0 para voltar ao seu menu)"
        matricula <- readLn
        if matricula == 0 then return () else do
            alunoExiste <- checaExistenciaById "Alunos" matricula
            monitorExisteNaDisciplina <- analisaAlunoComoMonitor matricula disciplina
            if monitorExisteNaDisciplina then putStrLn "Este aluno é monitor de sua disciplina!"
            else if alunoExiste then do
                    aluno <- getAluno matricula
                    if disciplina `elem` (A.disciplinas aluno) then putStrLn "Este aluno já está na sua diciplina!"
                    else do
                        let alunoAtualizado = Aluno (A.id aluno) (nome aluno) (disciplina : disciplinas aluno)
                        atualizaLinhaById "Alunos" (show matricula) (show alunoAtualizado)
                        putStrLn "Este aluno já está presente no sistema. Cadastro de aluno na disciplina realizado!"
            else do
                putStrLn "Este aluno não está cadastrado no SAD. Por favor, informe seu nome:"
                nome <- getLine
                let aluno = Aluno matricula nome [disciplina]
                adicionaLinha "Alunos" $ show aluno
                putStrLn "Aluno cadastrado com sucesso e incluso na sua disciplina.\n"

    {- 
    Função que realiza a matricula de um aluno em uma disciplina a partir das entradas do usuário
    Parametros:
        aluno = Aluno que deseja realizar a matricula
    -}
    matriculaAlunoEmDisciplina :: Aluno -> IO()
    matriculaAlunoEmDisciplina aluno = do
        putStrLn "\nEstas são todas as disciplinas disponíveis para matrícula:\n"
        exibeDisciplinasDisponiveis aluno
        putStrLn "\nInforme a sigla da disciplina na qual deseja se matricular:"
        sigla <- getLine
        siglasCadastradas <- getSiglas
        if sigla `elem` siglasCadastradas then
            if sigla `elem` disciplinas aluno then do
                putStrLn ("Você já está matriculado em " ++ sigla ++ ", tente novamente\n\n")
                matriculaAlunoEmDisciplina aluno
            else do
                let alunoAtualizado = Aluno (A.id aluno) (nome aluno) (sigla : disciplinas aluno)
                atualizaLinhaById "Alunos" (show (A.id aluno)) (show alunoAtualizado)
                putStrLn "Matricula realizada com sucesso!"
        else do
            putStrLn "Sigla Inválida , tente novamente.\n\n"
            matriculaAlunoEmDisciplina aluno

    {- 
    Função que desmatricula um aluno em alguma disciplina a partir de entradas do usuario (remove uma das disciplinas do array de disciplinas do aluno)
    Parametros:
        aluno = Aluno que deseja se desmatricular em alguma disciplina
    -}
    desmatriculaAlunoDeDisciplina :: Aluno -> IO()
    desmatriculaAlunoDeDisciplina aluno = do
        putStrLn "\nInforme a sigla da disciplina na qual deseja se desmatricular: "
        sigla <- getLine
        if sigla `elem` disciplinas aluno then do
            let disciplinasExcetoMencionada = filter (/= sigla) (disciplinas aluno)
            let alunoAtualizado = Aluno (A.id aluno) (nome aluno) disciplinasExcetoMencionada
            atualizaLinhaById "Alunos" (show (A.id aluno)) (show alunoAtualizado)
            putStrLn "Cancelamento de matricula realizada com sucesso!\n"
        else do
            putStrLn "Insira um valor válido!\n"
            desmatriculaAlunoDeDisciplina aluno
    
    {-
    Informa se um aluno cursa uma determinada disciplina.
    Parametros:
        matricula = a matricula do aluno
        disciplina = a sigla da disciplina a ser analisada
    -}
    alunoCursaDisciplina :: Int -> String -> IO(Bool)
    alunoCursaDisciplina matricula disciplina = do
        aluno <- getAluno matricula
        return (disciplina `elem` (A.disciplinas aluno))
    
    {-
    Verifica se a matrícula de um aluno é referente a um monitor de uma determinada disciplina
    Parametros:
        matricula = matricula a ser analisada
        disciplina = disciplina a ser verificada sobre a existencia do monitor
    -}
    analisaAlunoComoMonitor :: Int -> String -> IO(Bool)
    analisaAlunoComoMonitor matricula disciplina = do
        instanciaMonitor <- getObjetoById "Monitores" matricula
        if instanciaMonitor /= "" then do
            let monitor = read instanciaMonitor :: M.Monitor
            return (M.disciplina monitor == disciplina)
        else return False