module Src.Menu where

    import Src.Controller.AlunoController
    import Src.Controller.ChatController
    import Src.Controller.MonitorController
    import Src.Controller.ProfessorController
    import Src.Util.TxtFunctions
    import Src.Model.Monitor as M
    import Src.Model.Professor as P
    import Src.Model.Aluno as A

    menuPrincipal :: IO()
    menuPrincipal = do
        putStrLn "Bem vindo ao Sistema de Atendimento ao Discente!"
        putStrLn "O que deseja fazer?\n"
        putStrLn "1) Realizar cadastro\n2) Entrar no sistema\n3) Sair"
        opcao <- getLine
        putStr "\n"
        decideMenuPrincipal opcao
    
    decideMenuPrincipal :: String -> IO ()
    decideMenuPrincipal opcao
        | opcao == "1" = do
            menuCadastro
            menuPrincipal
        | opcao == "2" = menuLogin
        | opcao == "3" = putStrLn "Saindo..." 
        | otherwise = do 
            putStrLn "Insira um valor válido!\n"
            menuPrincipal

    menuCadastro :: IO ()
    menuCadastro = do
        putStrLn "Quem você deseja cadastrar?"
        putStrLn "1) Cadastrar aluno\n2) Cadastrar monitor\n3) Cadastrar professor\n4) Voltar para o menu principal"
        opcao <- getLine
        putStr "\n"
        if opcao == "1" then adicionaAluno else
            if opcao == "2" then adicionaMonitor else
                if opcao == "3" then adicionaProfessor else
                    if opcao == "4" then menuPrincipal else do
                    putStrLn "Insira um valor válido!\n"
                    menuCadastro

    menuLogin :: IO()
    menuLogin = do
        putStrLn "Por favor, insira sua matrícula (ou seu id, caso seja professor). Para voltar ao menu principal, digite VOLTAR:"

        input <- getLine
        putStr "\n"

        if input == "VOLTAR" then menuPrincipal else do
            let idPerfil = read input :: Int

            ehMonitor <- checaExistenciaById "Monitores" idPerfil
            ehProfessor <- checaExistenciaById "Professores" idPerfil
            ehAluno <- checaExistenciaById "Alunos" idPerfil

            decideMenuLogin idPerfil ehMonitor ehProfessor ehAluno

    decideMenuLogin :: Int -> Bool -> Bool -> Bool -> IO()
    decideMenuLogin idPerfil ehMonitor ehProfessor ehAluno = do
        if ehMonitor then decideMenuAlunoMonitor idPerfil else
            if ehAluno then exibeMenuAluno idPerfil else
                if ehProfessor then exibeMenuProfessor idPerfil else do
                    putStrLn "Matrícula/ID não encontrado. Tente novamente\n\n"
                    menuLogin

    decideMenuAlunoMonitor :: Int -> IO()
    decideMenuAlunoMonitor idPerfil = do
        instanciaMonitor <- buscaObjetoById "Monitores" idPerfil
        let monitor = read instanciaMonitor :: Monitor
        putStrLn ("Foi identificado que você é monitor da disciplina: " ++ disciplina monitor)
        putStrLn ("Como deseja entrar no sistema?\n\n1) Entrar como Aluno\n2) Entrar como Monitor de " ++ disciplina monitor)
        escolha <- getLine
        putStr "\n"
        if escolha == "1" then exibeMenuAluno idPerfil else 
            if escolha == "2" then exibeMenuMonitor idPerfil else do
                putStrLn "Insira um valor válido!"
                decideMenuAlunoMonitor idPerfil

    exibeMenuProfessor :: Int -> IO()
    exibeMenuProfessor idProfessor = do
        instanciaProfessor <- buscaObjetoById "Professores" idProfessor
        let professor = read instanciaProfessor :: Professor
        putStrLn "\n== SAD: MENU PROFESSOR =="
        putStrLn ("ID: " ++ show (P.id professor) ++ " | " ++ "Nome: " ++ P.nome professor ++ " | " ++ "Disciplinas: " ++ show (P.disciplinas professor))
        putStrLn "Digite o número da ação que deseja executar!\n"
        putStrLn "1) Exibir tickets\n2) Responder Tickets em progresso\n3) Desvincular Monitor\n4) Deslogar"
        opcao <- getLine
        decideMenuProfessor professor opcao

    decideMenuProfessor :: Professor -> String -> IO()
    decideMenuProfessor professor opcao
        | opcao == "1" = do
            lerTicketsDisciplina professor
            exibeMenuProfessor (P.id professor)
        | opcao == "2" = do
            putStrLn "Respondendo tickets...\n"
            exibeMenuProfessor (P.id professor)
        | opcao == "3" = do
            removeMonitor (P.disciplinas professor)
            exibeMenuProfessor (P.id professor)
        | opcao == "4" = do
            putStrLn "Deslogando...\n"
            menuPrincipal
        | otherwise = do 
            putStrLn "Opção inválida!"
            exibeMenuProfessor (P.id professor)

    exibeMenuMonitor :: Int -> IO()
    exibeMenuMonitor idMonitor = do
        instanciaMonitor <- buscaObjetoById "Monitores" idMonitor
        instanciaMonitorAluno <- buscaObjetoById "Alunos" idMonitor
        let monitor = read instanciaMonitor :: Monitor
        let aluno = read instanciaMonitorAluno :: Aluno
        putStrLn "\n== SAD: MENU MONITOR =="
        putStrLn ("ID: " ++ show (M.id monitor) ++ " | " ++ "Nome: " ++ A.nome aluno ++ " | " ++ "Disciplina: " ++ M.disciplina monitor)
        putStrLn "Digite o número da ação que deseja executar!\n"
        putStrLn "1) Exibir todos os tickets\n2) Responder tickets em progresso\n3) Deslogar"
        opcao <- getLine 
        decideMenuMonitor monitor opcao

    decideMenuMonitor :: Monitor -> String -> IO()
    decideMenuMonitor monitor opcao
        | opcao == "1" = do
            exibeTicketsDisciplina (disciplina monitor)
            exibeMenuMonitor (M.id monitor)
        | opcao == "2" = do
            respondeTicket monitor
            exibeMenuMonitor (M.id monitor)
        | opcao == "3" = do
            putStrLn "Deslogando...\n"
            menuPrincipal
        | otherwise  = do
            putStrLn "Opção inválida!\n"
            exibeMenuMonitor (M.id monitor)

    exibeMenuAluno :: Int -> IO()
    exibeMenuAluno idAluno = do
        putStrLn "\n== SAD: MENU ALUNO ==\nDigite o número da ação que deseja executar!\n\n"
        instanciaAluno <- buscaObjetoById "Alunos" idAluno
        let aluno = read instanciaAluno :: Aluno
        putStrLn "1) Matricular-se em disciplina\n2) Desmatricular-se de disciplina\n3) Criar Ticket\n4) Mandar mensagem em um ticket\n5) Ler tickets de uma disciplina\n6) Marcar ticket como resolvido\n7)Deslogar"
        opcao <- getLine 
        decideMenuAluno aluno opcao
    
    decideMenuAluno :: Aluno -> String -> IO()
    decideMenuAluno aluno opcao
        | opcao == "1" = do
            matriculaAlunoEmDisciplina aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "2" = do
            desmatriculaAlunoDeDisciplina aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "3" = do
            adicionaTicket aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "4" = do
            adicionaMensagemAluno aluno
            menuPrincipal
        | opcao == "5" = do
            -- Falta funcao para ler tickets de uma disciplina?
            putStrLn "Deslogando...\n"
            menuPrincipal
        | opcao == "6" = do
            resolveTicket aluno
            menuPrincipal
        | opcao == "7" = do
            putStrLn "Deslogando...\n"
            menuPrincipal
        | otherwise  = do
            putStrLn "Opção inválida!\n"
            exibeMenuAluno (A.id aluno)