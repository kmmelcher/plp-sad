import System.IO
import Data.List.Split
-- TODO Fix nomes das funções

-- Essa funcao adiciona uma linha no arquivo, caso o arquivo já possua uma linha,
-- a funcao irá adicionar na linha seguinte
-- Parametros:
--    line = Precisa ser no formato e na ordem do csv "text,text,text,text" 
--    arq = caminho do arquivo no diretório
adicionaLinhaCsv :: FilePath -> String -> IO() 
adicionaLinhaCsv arq line = do
    let mensagem = line ++ "\n"
    appendFile arq mensagem
  

-- Remove todas as linhas setando-as para uma linha vazia
-- USE COM CUIDADO
-- Parametros: 
--    arq = caminho do arquivo no diretório 
--    contemHeader = booleano que indica a existencia de um header no arquivo
-- removeTodasAsLinhas :: FilePath -> Bool  -> IO()
-- removeTodasAsLinhas arq False = do
--     arquivo <- openFile arq WriteMode
--     hPutStr arquivo ""
--     hFlush arquivo
--     hClose arquivo
-- removeTodasAsLinhas arq contemHeader = do
-- --    arquivo <- openFile arq ReadWriteMode
-- --    if contemHeader
-- --        then let id = "header"
-- --    hPutStr arquivo ""
 

-- Remove uma linha especifica do arquivo
-- Parâmetros:
--     numID = tamanho do ID do campo id em Inteiro
--     id = id da linha a ser eliminada
--     arq = caminho do arquivo
removeLinhaCsv :: Int -> String -> FilePath -> IO ()
removeLinhaCsv idLength id arq = do 
    arquivo <- openFile arq ReadWriteMode
    deletaLinha idLength id arquivo 
    hFlush arquivo
    hClose arquivo

--removeLinhaCsv idLegth id arq = do
--    content <- readFile arq
--    let contentAbstract = splitOn ("\n") (content)
--    print contentAbstract




-- Remove uma linha especifica do arquivo dentro do csv
-- Parâmetros:
--     idLength= tamanho do ID do campo id em Inteiro
--     id = id da linha a ser eliminada
--     inh = caminho do arquivo
deletaLinha :: Int -> String -> Handle -> IO ()
deletaLinha idLength id inh = 
    do ineof <- hIsEOF inh
       if ineof
          then return ()
           else removeComBaseEmID idLength id inh

removeComBaseEmID :: Int -> String -> Handle -> IO()
removeComBaseEmID idLength id file = do 
                inpStr <- hGetLine file
                let idCSV = take idLength inpStr
                print idCSV
                print id
                if id == idCSV 
                   then do
                    print "entrooou"
                    hPutStrLn file "teste0000"
                    deletaLinha idLength id file 
                else  
                   deletaLinha idLength id file

-- Teste
main :: IO()
main = do 
  adicionaLinhaCsv "../database/alunos.csv" "teste"