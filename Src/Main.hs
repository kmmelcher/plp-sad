import Src.Controller.AlunoController
import Src.Controller.ChatController
import Src.Controller.MonitorController
import Src.Controller.ProfessorController
import Src.Util.TxtFunctions (checaExistencia, buscaObjetoById)
import Src.Model.Monitor
import Src.Model.Professor
import Src.Model.Aluno
import System.Exit

main :: IO()
main = do
    putStrLn "Bem vindo ao Sistema de Atendimento ao Discente!"
    putStrLn "O que deseja fazer?\n"
    putStrLn "1) Realizar cadastro\n2) Entrar no sistema\n3) Sair"
    opcao <- getLine
    putStr "\n"
    if opcao == "1" then do
        menuCadastro
        main
        else 
        if opcao == "2" then menuLogin else 
            if opcao == "3" then exitSuccess else do
            putStrLn "Insira um valor válido!\n"
            main

menuCadastro :: IO ()
menuCadastro = do
    putStrLn "Como deseja se cadastrar?"
    putStrLn "1) Cadastrar como aluno\n2) Cadastrar como monitor\n3) Cadastrar como professor\n4) Voltar para o menu principal"
    opcao <- getLine
    putStr "\n"
    if opcao == "1" then adicionaAluno else
        if opcao == "2" then adicionaMonitor else
            if opcao == "3" then adicionaProfessor else
                if opcao == "4" then main else do
                putStrLn "Insira um valor válido!\n"
                menuCadastro

menuLogin :: IO()
menuLogin = do
    putStrLn "Por favor, insira sua matrícula (ou seu id, caso seja professor). Para voltar ao menu principal, digite VOLTAR:"

    input <- getLine
    putStr "\n"

    if input == "VOLTAR" then main else do
        let idPerfil = read input :: Int

        ehMonitor <- checaExistencia "Monitores" idPerfil
        ehProfessor <- checaExistencia "Professores" idPerfil
        ehAluno <- checaExistencia "Alunos" idPerfil

        decideMenu idPerfil ehMonitor ehProfessor ehAluno

decideMenu :: Int -> Bool -> Bool -> Bool -> IO()
decideMenu idPerfil ehMonitor ehProfessor ehAluno = do
    if ehMonitor then decideMenuAlunoMonitor idPerfil else
        if ehAluno then exibeMenuAluno idPerfil else
            if ehProfessor then exibeMenuProfessor idPerfil else do
                putStrLn "Matrícula/ID não encontrado. Tente novamente\n\n"
                menuLogin

decideMenuAlunoMonitor :: Int -> IO()
decideMenuAlunoMonitor idPerfil = do
    instanciaMonitor <- (buscaObjetoById "Monitores" idPerfil)
    let monitor = read instanciaMonitor :: Monitor
    putStrLn ("Foi identificado que você é monitor da disciplina: " ++ disciplina monitor)
    putStrLn ("Como deseja entrar no sistema?\n\n 1) Entrar como Aluno\n 2) Entrar como Monitor de " ++ disciplina monitor)
    escolha <- getLine
    if escolha == "1" then exibeMenuAluno idPerfil else 
        if escolha == "2" then exibeMenuMonitor idPerfil else do
            putStrLn "Insira um valor válido!"
            decideMenuAlunoMonitor idPerfil

exibeMenuProfessor :: Int -> IO()
exibeMenuProfessor idProfessor = do
    putStrLn "== SAD: MENU PROFESSOR ==\n Digite o número da ação que deseja executar!\n\n"
    instanciaProfessor <- buscaObjetoById "Professores" idProfessor
    let professor = read instanciaProfessor :: Professor
    putStrLn "1) Exibir tickets\n2) Responder Tickets em progresso\n3) Desvincular Monitor"

exibeMenuMonitor :: Int -> IO()
exibeMenuMonitor idMonitor = do
    putStrLn "== SAD: MENU MONITOR ==\n Digite o número da ação que deseja executar!\n\n"
    instanciaMonitor <- buscaObjetoById "Monitores" idMonitor
    let monitor = read instanciaMonitor :: Monitor
    putStrLn "1) Exibir todos os tickets\n Responder tickets em progresso"

exibeMenuAluno :: Int -> IO()
exibeMenuAluno idAluno = do
    putStrLn "== SAD: MENU ALUNO ==\n Digite o número da ação que deseja executar!\n\n"
    instanciaAluno <- buscaObjetoById "Alunos" idAluno
    let aluno = read instanciaAluno :: Aluno
    putStrLn "1) Matricular-se em disciplina\n2) Desmatricular-se de disciplina\n3) Criar Ticket\n4) Mandar mensagem em um ticket\n5) Ler tickets de uma disciplina\n6) Marcar ticket como resolvido"