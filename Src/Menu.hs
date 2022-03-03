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
            adicionaAluno disciplina
        | opcao == "2" = do
            disciplina <- solicitaDisciplina professor
            adicionaMonitor disciplina
        | opcao == "3" = putStrLn ""
        | otherwise = do
            putStrLn "Insira um valor válido!\n"
            menuCadastro professor

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
        | ehMonitor = decideMenuAlunoMonitor idPerfil
        | ehAluno = exibeMenuAluno idPerfil
        | ehProfessor = exibeMenuProfessor idPerfil
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
        putStrLn "1) Exibir tickets\n2) Responder Tickets em progresso\n3) Vincular aluno/monitor\n4) Desvincular aluno/monitor\n5) Deslogar"
        opcao <- getLine
        decideMenuProfessor professor opcao

    decideMenuProfessor :: Professor -> String -> IO()
    decideMenuProfessor professor opcao
        | opcao == "1" = do
            lerTicketsDisciplinaProfessor professor
            exibeMenuProfessor (P.id professor)
        | opcao == "2" = do
            putStrLn "Respondendo tickets...\n"
            exibeMenuProfessor (P.id professor)
        | opcao == "3" = do
            menuCadastro professor
            exibeMenuProfessor (P.id professor)
        | opcao == "4" = do
            removeMonitor (P.disciplinas professor)
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
        putStrLn "1) Matricular-se em disciplina\n2) Desmatricular-se de disciplina\n3) Ler tickets de uma disciplina\n4) Ler meus tickets\n5) Criar Ticket\n6) Mandar mensagem em um ticket meu\n7) Marcar ticket como resolvido\n8) Excluir ticket\n9) Deslogar"
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
            leTicketsDaDisciplinaAluno aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "4" = do
            leTicketsDoAluno aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "5" = do
            adicionaTicket aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "6" = do
            adicionaMensagemAluno aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "7" = do
            resolveTicket aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "8" = do
            excluirTicket aluno
            exibeMenuAluno (A.id aluno)
        | opcao == "9" = do
            putStrLn "Deslogando...\n"
            menuPrincipal
        | otherwise  = do
            putStrLn "Opção inválida!\n"
            exibeMenuAluno (A.id aluno)