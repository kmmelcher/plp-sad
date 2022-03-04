--Modulo com funções de criptografia para as senhas
module Util.EncriptFunctions where
    import Data.Char
    import System.Environment
    import System.IO

    -- Função que retorna uma chave compativel com a senha
    --  > Parametros
    --    msg = senha a ser utilizada
    --    chave = chave a ser utilizada
    prepareChave :: [Char] -> [Char] -> [Char]
    prepareChave msg chave = take (length msg) (cycle chave)

    -- Converte um array de char para um array de inteiros a partir da tabela ASCII
    --  > Parametros
    --    arr = array de chars a serem convertidos 
    convertString :: [Char] -> [Int]
    convertString = map ord

    -- Converte um array de inteiros para um array de chars(string) a partir da tabela ASCII
    --  > Parametros
    --    arr = array de ints a serem convertidos 
    transformaEmString :: [Int] -> String
    transformaEmString = concatMap show

    -- Encripta uma entrada a partir de uma chave
    --  > Parametros
    --    entrada = mensagem a ser encriptada
    --    chave = chave para criptografia 
    encripta :: String -> String -> String
    encripta entrada chave = do
        transformaEmString $ zipWith (*) (convertString entrada) (convertString (prepareChave entrada chave))