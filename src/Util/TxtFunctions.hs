module Util.TxtFunctions where
    import System.IO
    import Control.Exception (evaluate)

    {- 
    Essa funcao adiciona uma linha no arquivo, caso o arquivo já possua uma linha,
    a funcao irá adicionar na linha seguinte
    Parametros:
        conteudo = Precisa ser no formato e na ordem do csv 
        path = caminho do arquivo no diretório
    -}
    adicionaLinha :: FilePath -> String -> IO() 
    adicionaLinha path conteudo = do
        let mensagem = conteudo ++ "\n"
        appendFile path mensagem

    {-
    Essa funcao atualiza uma linha do csv com base no seu id
    Parametros:
        path = caminho do arquivo no diretório
        id = id da linha 
        novaLinha = conteudo da nova linha a ser atualizada
    -}
    atualizaLinha :: FilePath -> String -> String -> IO()
    atualizaLinha path id novaLinha = do
        conteudoArquivo <- leConteudoGeral path
        arquivo <- openFile path WriteMode
        hPutStr arquivo ""
        atualizaArquivo conteudoArquivo id novaLinha arquivo
        hFlush arquivo
        hClose arquivo

    {-
    Essa funcao lê todo o conteúdo do arquivo e retorna uma string com todo o conteúdo do arquivo
    Parametros:
        path = caminho do arquivo no diretório
    -}
    leConteudoGeral :: FilePath -> IO([String]) 
    leConteudoGeral path = do
        arquivo <- openFile path ReadMode
        conteudo <- hGetContents arquivo
        evaluate (length conteudo)
        let conteudoEmLista = lines conteudo
        return conteudoEmLista

    {-
    Essa funcao remove uma linha com base no id da linha
    Parametros:
        path = caminho do arquivo no diretório
        id = id da linha 
    -}
    removeLinhaCsv :: FilePath -> String -> IO ()
    removeLinhaCsv path id = do
        conteudoArquivo <- leConteudoGeral path
        arquivo <- openFile path WriteMode
        hPutStr arquivo ""
        atualizaArquivo conteudoArquivo id "" arquivo
        hFlush arquivo
        hClose arquivo

    {-
    Essa funcao atualiza todo o arquivo csv linha a linha, podendo não fazer nada
    caso seja uma chamada vazia. Somente remover a linha, ou atualizar uma linha do arquivo.
    Parametros:
        (linhaAtual:linhasRestantes) = conteúdo do arquivo a ser atualizado [sem header]
        id = id da linha 
        novaLinha = conteudo da nova linha a ser atualizada ["" se for para uma remoção]
    -}
    atualizaArquivo:: [String] -> String -> String -> Handle  -> IO ()
    atualizaArquivo [] _ _ _= return ()
    atualizaArquivo (linhaAtual:linhasRestantes) id "" arquivo = do
        if take (length id) linhaAtual /= id 
            then do 
                hPutStrLn arquivo linhaAtual
                atualizaArquivo linhasRestantes id "" arquivo
            else atualizaArquivo linhasRestantes id "" arquivo

    atualizaArquivo (linhaAtual:linhasRestantes) id novaLinha arquivo = do
        if take (length id) linhaAtual /= id 
            then do 
                hPutStrLn arquivo linhaAtual
                atualizaArquivo linhasRestantes id novaLinha arquivo
            else do
                hPutStrLn arquivo novaLinha 
                atualizaArquivo linhasRestantes id novaLinha arquivo
