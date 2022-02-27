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
    if opcao == "1" then adicionaAluno else
        if opcao == "2" then adicionaMonitor else
            if opcao == "3" then adicionaProfessor else
                if opcao == "4" then main else do
                putStrLn "Insira um valor válido!\n"
                main

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
    instanciaProfessor <- buscaObjetoById "Professores" idProfessor
    let professor = read instanciaProfessor :: Professor
    putStrLn "Menu do professor ser construido..."

exibeMenuMonitor :: Int -> IO()
exibeMenuMonitor idMonitor = do
    instanciaMonitor <- buscaObjetoById "Monitores" idMonitor
    let monitor = read instanciaMonitor :: Monitor
    putStrLn "Menu do monitor a ser construido..."

exibeMenuAluno :: Int -> IO()
exibeMenuAluno idAluno = do
    instanciaAluno <- buscaObjetoById "Alunos" idAluno
    let aluno = read instanciaAluno :: Aluno
    print "Menu do aluno a ser construido..."