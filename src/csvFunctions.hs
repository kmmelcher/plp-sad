import System.IO
    ( hPutStrLn,
      hClose,
      hFlush,
      openFile,
      IOMode(WriteMode, ReadMode),
      Handle,
      hGetContents, hPutStr )

-- TODO Fix nomes das funções

-- Essa funcao adiciona uma linha no arquivo, caso o arquivo já possua uma linha,
-- a funcao irá adicionar na linha seguinte
-- Parametros:
--    line = Precisa ser no formato e na ordem do csv "text,text,text,text" 
--    arq = caminho do arquivo no diretório
adicionaLinhaCsv :: FilePath -> String -> IO() 
adicionaLinhaCsv path conteudo = do
    let mensagem = conteudo ++ "\n"
    appendFile path mensagem

leConteudoGeralCsv :: FilePath -> IO([String]) 
leConteudoGeralCsv path = do
    arquivo <- openFile path ReadMode
    conteudo <- hGetContents arquivo
    let conteudoEmLista = lines conteudo
    print conteudoEmLista 
    hClose arquivo
    return conteudoEmLista

removeLinhaCsv :: FilePath -> String -> IO ()
removeLinhaCsv path id = do
    conteudoArquivo <- leConteudoGeralCsv path

    arquivo <- openFile path WriteMode
    hPutStr arquivo ""
    atualizaArquivo conteudoArquivo id arquivo
    hFlush arquivo
    hClose arquivo

atualizaArquivo:: [String] -> String -> Handle  -> IO ()
atualizaArquivo [] _ _= return ()
atualizaArquivo (linhaAtual:linhasRestantes) id arquivo = do
    if take (length id) linhaAtual /= id 
        then do 
            hPutStrLn arquivo linhaAtual
            atualizaArquivo linhasRestantes id arquivo
        else atualizaArquivo linhasRestantes id arquivo

main :: IO()
main = do 
  removeLinhaCsv "../database/alunos.csv" "120110407"
  adicionaLinhaCsv "../database/alunos.csv" "120110338,Vinicus Azevedo,\"oac,loac\""