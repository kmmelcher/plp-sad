module Util.TxtFunctions where
    import System.IO
    import Control.Exception (evaluate)

    {-
    Esta função retorna um array de string com todo o conteúdo do arquivo tendo quebras de linha como separador.
    Parametros:
        path = caminho do arquivo no diretório
    -}
    fileToStringArray :: FilePath -> IO([String]) 
    fileToStringArray path = do
        arquivo <- openFile path ReadMode
        conteudo <- hGetContents arquivo
        evaluate (length conteudo)
        let conteudoEmLista = lines conteudo
        return conteudoEmLista

    
    -- INSERIR AQUI: LER CONTEUDO ESPECIFICO

    {- 
    Esta função adiciona uma linha no arquivo. Caso o arquivo já possua uma linha,
    a funcao irá adicionar na linha seguinte
    Parametros:
        conteudo = Precisa ser no formato string de um objeto 
        path = caminho do arquivo no diretório
    -}
    adicionaLinha :: FilePath -> String -> IO()
    adicionaLinha path conteudo = do
        let mensagem = conteudo ++ "\n"
        appendFile path mensagem

    {-
    Essa funcao atualiza uma linha do arquivo com base no seu id
    Parametros:
        path = caminho do arquivo no diretório
        id = id da linha 
        novaLinha = conteudo da nova linha a ser atualizada
    -}
    atualizaLinhaById :: FilePath -> String -> String -> IO()
    atualizaLinhaById path id novaLinha = do
        conteudoArquivo <- fileToStringArray path
        arquivo <- openFile path WriteMode
        hPutStr arquivo ""
        atualizaLista conteudoArquivo id novaLinha arquivo
        hFlush arquivo
        hClose arquivo
    
    {-
    Esta funcao atualiza uma linha que contem um id declarado . Somente remover a linha, ou atualizar uma linha do arquivo.
    Parametros:
        (linhaAtual:linhasRestantes) = conteúdo do arquivo a ser atualizado [sem header]
        id = id da linha 
        novaLinha = conteudo da nova linha a ser atualizada ["" se for para uma remoção]
    -}
    atualizaLista :: [String] -> String -> String -> Handle  -> IO ()
    atualizaLista [] _ _ _= return ()
    atualizaLista (linhaAtual:linhasRestantes) id novaLinha arquivo = do
        if take (length id) linhaAtual /= id 
            then do 
                hPutStrLn arquivo linhaAtual
                atualizaLista linhasRestantes id novaLinha arquivo
            else do
                hPutStrLn arquivo novaLinha 
                atualizaLista linhasRestantes id novaLinha arquivo
    
    {-
    Essa funcao remove uma linha com base no id da linha
    Parametros:
        path = caminho do arquivo no diretório
        id = id da linha 
    -}
    removeLinha :: FilePath -> String -> IO ()
    removeLinha path id = do
        conteudoArquivo <- fileToStringArray path
        arquivo <- openFile path WriteMode
        hPutStr arquivo ""
        atualizaLista conteudoArquivo id "" arquivo
        hFlush arquivo
        hClose arquivo