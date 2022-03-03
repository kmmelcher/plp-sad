module Src.Controller.AlunoController where
    import Src.Model.Aluno as A
    import Src.Util.TxtFunctions
    import Src.Controller.DisciplinaController as DC
    

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
    adicionaAluno :: IO()
    adicionaAluno = do
        putStrLn "Insira o nome do aluno: "
        nome <- getLine
        putStrLn "Insira a matricula do aluno"
        matricula <- readLn
        putStrLn "Insira as disciplinas do aluno"
        disciplinas <- readLn
        let aluno = Aluno matricula nome disciplinas
        adicionaLinha "Alunos" $ show aluno
        putStrLn "Aluno cadastrado com sucesso.\n"

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