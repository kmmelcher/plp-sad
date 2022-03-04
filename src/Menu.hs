module Menu where

    import Controller.AlunoController
    import Controller.ChatController
    import Controller.MonitorController
    import Controller.ProfessorController
    import Util.TxtFunctions
    import Util.EncriptFunctions
    import Model.Monitor as M
    import Model.Professor as P
    import Model.Aluno as A

    menuPrincipal :: IO()
    menuPrincipal = do
        putStrLn "\nBem vindo ao SAD: Sistema de Atendimento ao Discente! :):"
        menuLogin

    decideMenuPrincipal :: String -> IO ()
    decideMenuPrincipal opcao
        | opcao == "1" = menuLogin
        | opcao == "2" = putStrLn "Saindo..."
        | otherwise = do
            putStrLn "Insira um valor válido!\n"
            menuPrincipal

    menuCadastro :: Professor -> IO ()
    menuCadastro professor = do
        putStrLn "\nQuem você deseja vincular?"
        putStrLn "1) Vincular aluno\n2) Vincular monitor\n3) Voltar para o menu professor"
        opcao <- getLine
        putStr "\n"
        decideMenuCadastro professor opcao

    decideMenuCadastro :: Professor -> String -> IO ()
    decideMenuCadastro professor opcao
        | opcao == "1" = do
            disciplina <- solicitaDisciplina professor
            vinculaAluno disciplina
        | opcao == "2" = do
            disciplina <- solicitaDisciplina professor
            vinculaMonitor disciplina
        | opcao == "3" = putStrLn ""
        | otherwise = do
            putStrLn "Insira um valor válido!\n"
            menuCadastro professor

    menuRemocao :: Professor -> IO()
    menuRemocao professor = do
        putStrLn "\nQuem você deseja desvincular?"
        putStrLn "1) Desvincular aluno\n2) Desvincular monitor\n3) Voltar para o menu professor"
        opcao <- getLine
        putStr "\n"
        decideMenuRemocao professor opcao

    decideMenuRemocao :: Professor -> String -> IO()
    decideMenuRemocao professor opcao
        | opcao == "1" = do
            disciplina <- solicitaDisciplina professor
            desvinculaAluno disciplina
        | opcao == "2" = do
            disciplina <- solicitaDisciplina professor
            desvinculaMonitor disciplina
        | opcao == "3" = putStrLn ""
        | otherwise = do
            putStrLn "Insira um valor válido!\n"
            menuRemocao professor

    menuLogin :: IO()
    menuLogin = do
        putStrLn "Insira seu ID para entrar. Para sair do sistema, digite SAIR:"

        input <- getLine
        putStr "\n"

        if input == "SAIR" then putStrLn "Saindo..." else do
            let idPerfil = read input :: Int

            ehMonitor <- checaExistenciaById "Monitores" idPerfil
            ehProfessor <- checaExistenciaById "Professores" idPerfil
            ehAluno <- checaExistenciaById "Alunos" idPerfil

            decideMenuLogin idPerfil ehMonitor ehProfessor ehAluno

    decideMenuLogin :: Int -> Bool -> Bool -> Bool -> IO()
    decideMenuLogin idPerfil ehMonitor ehProfessor ehAluno
        | ehMonitor = do
            decideMenuAlunoMonitor idPerfil
        | ehAluno = do
            aluno <- getAluno idPerfil
            autenticacao <- autenticaAluno aluno
            if autenticacao then exibeMenuAluno idPerfil
                else do
                    putStrLn "Senha incorreta\n"
                    menuLogin
        | ehProfessor = do
            exibeMenuProfessor idPerfil
        | otherwise = do
            putStrLn "Matrícula/ID não encontrado. Tente novamente\n\n"
            menuLogin

    decideMenuAlunoMonitor :: Int -> IO()
    decideMenuAlunoMonitor idPerfil = do
        monitor <- getMonitor idPerfil
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
        professor <- getProfessor idProfessor
        putStrLn "\n== SAD: MENU PROFESSOR =="
        putStrLn ("ID: " ++ show (P.id professor) ++ " | " ++ "Nome: " ++ P.nome professor ++ " | " ++ "Disciplinas: " ++ show (P.disciplinas professor))
        putStrLn "Digite o número da ação que deseja executar!\n"
        putStrLn "1) Exibir tickets\n2) Responder Tickets em andamento\n3) Vincular aluno/monitor\n4) Desvincular aluno/monitor\n5) Deslogar"
        opcao <- getLine
        decideMenuProfessor professor opcao

    decideMenuProfessor :: Professor -> String -> IO()
    decideMenuProfessor professor opcao
        | opcao == "1" = do
            lerTicketsDisciplinaProfessor professor
            exibeMenuProfessor (P.id professor)
        | opcao == "2" = do
            adicionaMensagemProfessor professor
            exibeMenuProfessor (P.id professor)
        | opcao == "3" = do
            menuCadastro professor
            exibeMenuProfessor (P.id professor)
        | opcao == "4" = do
            menuRemocao professor
            exibeMenuProfessor (P.id professor)
        | opcao == "5" = do
            putStrLn "Deslogando...\n"
            menuPrincipal
        | otherwise = do
            putStrLn "Opção inválida!"
            exibeMenuProfessor (P.id professor)

    exibeMenuMonitor :: Int -> IO()
    exibeMenuMonitor id = do
        monitor <- getMonitor id
        aluno <- getAluno id
        putStrLn "\n== SAD: MENU MONITOR =="
        putStrLn ("ID: " ++ show (M.id monitor) ++ " | " ++ "Nome: " ++ A.nome aluno ++ " | " ++ "Disciplina: " ++ M.disciplina monitor)
        putStrLn "Digite o número da ação que deseja executar!\n"
        putStrLn "1) Exibir todos os tickets\n2) Responder tickets em andamento\n3) Deslogar"
        opcao <- getLine
        decideMenuMonitor monitor opcao

    decideMenuMonitor :: Monitor -> String -> IO()
    decideMenuMonitor monitor opcao
        | opcao == "1" = do
            exibeMensagensDisciplina (disciplina monitor)
            exibeMenuMonitor (M.id monitor)
        | opcao == "2" = do
            adicionaMensagemMonitor monitor
            exibeMenuMonitor (M.id monitor)
        | opcao == "3" = do
            putStrLn "Deslogando...\n"
            menuPrincipal
        | otherwise  = do
            putStrLn "Opção inválida!\n"
            exibeMenuMonitor (M.id monitor)

    exibeMenuAluno :: Int -> IO()
    exibeMenuAluno idAluno = do
        aluno <- getAluno idAluno
        putStrLn "\n== SAD: MENU ALUNO =="
        putStrLn ("ID: " ++ show (A.id aluno) ++ " | " ++ "Nome: " ++ A.nome aluno ++ " | " ++ "Disciplinas: " ++ show (A.disciplinas aluno))
        putStrLn "Digite o número da ação que deseja executar!\n"
        putStrLn "1) Ler tickets de uma disciplina\n2) Ler meus tickets\n3) Criar Ticket\n4) Mandar mensagem em um ticket meu\n5) Marcar ticket como resolvido\n6) Excluir ticket\n7) Deslogar"
        opcao <- getLine
        decideMenuAluno aluno opcao

    decideMenuAluno :: Aluno -> String -> IO()
    decideMenuAluno aluno opcao
        | opcao == "1" = do
            leTicketsDaDisciplinaAluno aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "2" = do
            leTicketsDoAluno aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "3" = do
            adicionaTicket aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "4" = do
            adicionaMensagemAluno aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "5" = do
            resolveTicket aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "6" = do
            excluirTicket aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "7" = do
            putStrLn "Deslogando...\n"
            menuPrincipal
        | otherwise  = do
            putStrLn "Opção inválida!\n"
            exibeMenuAluno (A.id aluno)

    autenticaAluno :: Aluno -> IO Bool
    autenticaAluno aluno = do
        putStrLn "Insira a senha de acesso:"
        senha <- getLine
        return (A.senha aluno == encripta senha (A.nome aluno))