import Src.Controller

realizaOperacao:: String -> IO()
realizaOperacao operacao
    | operacao == "1" = adicionaAluno
    | operacao == "2" = adicionaProfessor 
    | operacao == "4" = adicionaTicket 
    | otherwise = putStrLn "Operação inválida!"

main :: IO()
main = do
    putStrLn "Bem vindo ao SAD!\n----- MENU -----"
    putStrLn "1) Cadastrar aluno"
    putStrLn "2) Cadastrar Professor"
    putStrLn "3) Cadastrar Monitor"
    putStrLn "4) Adicionar um novo ticket"
    operacao <- getLine 
    realizaOperacao operacao
