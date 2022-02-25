import Src.Controller.AlunoController
import Src.Controller.ChatController
import Src.Controller.MonitorController
import Src.Controller.ProfessorController

realizaOperacao:: String -> IO()
realizaOperacao operacao
    | operacao == "1" = adicionaAluno
    | operacao == "2" = adicionaProfessor 
    -- | operacao == "3" = adicionaMonitor
    | operacao == "4" = adicionaTicket 
    | operacao == "5" = adicionaMensagem  
    | otherwise = putStrLn "Operação inválida!"

main :: IO()
main = do
    putStrLn "Bem vindo ao SAD!\n----- MENU -----"
    putStrLn "1) Cadastrar aluno"
    putStrLn "2) Cadastrar Professor"
    putStrLn "3) Cadastrar Monitor"
    putStrLn "4) Adicionar um novo ticket"
    putStrLn "5) Adicionar uma mensagem em ticket"
    operacao <- getLine 
    realizaOperacao operacao
