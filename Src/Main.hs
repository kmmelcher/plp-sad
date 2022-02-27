import Src.Controller.AlunoController
import Src.Controller.ChatController
import Src.Controller.MonitorController
import Src.Controller.ProfessorController
import Src.Util.TxtFunctions (checaExistencia, buscaObjetoById)
import Src.Model.Monitor
import Src.Model.Professor
import Src.Model.Aluno

main :: IO()
main = do
    putStrLn "Bem vindo ao Sistema de Atendimento Discente!\n"
    putStrLn "Por favor, insira sua matrícula (ou seu id, caso seja professor):"

    idInput <- getLine
    putStr "\n"
    let idPerfil = read idInput :: Int

    ehMonitor <- checaExistencia "Monitores" idPerfil
    ehProfessor <- checaExistencia "Professores" idPerfil
    ehAluno <- checaExistencia "Alunos" idPerfil

    decideMenu idPerfil ehMonitor ehProfessor ehAluno

decideMenu :: Int -> Bool -> Bool -> Bool -> IO()
decideMenu idPerfil ehMonitor ehProfessor ehAluno = do
    if ehMonitor then decideMenuAlunoMonitor idPerfil
        else
            if ehAluno then exibeMenuAluno idPerfil
            else
                if ehProfessor then exibeMenuProfessor idPerfil
                else do
                    putStrLn "Matrícula/ID não encontrado. Tente novamente\n"
                    main

decideMenuAlunoMonitor :: Int -> IO()
decideMenuAlunoMonitor idPerfil = do
    instanciaMonitor <- (buscaObjetoById "Monitores" idPerfil)
    let monitor = read instanciaMonitor :: Monitor
    putStrLn ("Foi identificado que você é monitor da disciplina: " ++ disciplina monitor)
    putStrLn "Como você deseja entrar no sistema?\n\n 1) Logar como Aluno\n 2) Logar como Monitor"
    escolha <- getLine
    if escolha == "1" then exibeMenuAluno idPerfil
        else 
            if escolha == "2" then exibeMenuMonitor idPerfil
            else do
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