import Src.Controller

realizaOperacao:: String -> IO()
realizaOperacao operacao
    | operacao == "1" = createAluno
    | otherwise = putStrLn "Operação inválida!"

main :: IO()
main = do
    putStrLn "Bem vindo ao SAD!\n----- MENU -----"
    putStrLn "1) Cadastrar aluno"
    operacao <- getLine 
    realizaOperacao operacao
