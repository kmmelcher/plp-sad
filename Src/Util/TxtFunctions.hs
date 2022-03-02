module Src.Util.TxtFunctions where
    import System.IO
    import Control.Exception (evaluate)
    import Prelude as P
    import qualified Data.List as T


    {-
    Esta função retorna um array de string com todo o conteúdo do arquivo tendo quebras de linha 
    como separador.
    Parametros:
        nomeArquivo = nome do arquivo no diretório database
    -}
    fileToStringArray :: String -> IO([String])
    fileToStringArray nomeArquivo = do
        arquivo <- openFile ("database/" ++ nomeArquivo ++ ".txt") ReadMode
        conteudo <- hGetContents arquivo
        evaluate (P.length conteudo)
        let conteudoEmLista = P.lines conteudo
        return conteudoEmLista

    {- 
    Esta função retorna o formato em string de um objeto identificado pelo seu id. 
    Caso não seja encontrado o objeto, uma string vazia é retornada.
    Parametros:
        nomeArquivo = o nome do arquivo no diretório database no qual o objeto se encontra
        idObjeto = O id do objeto que deseja ser buscado
    -}
    buscaObjetoById :: String -> Int -> IO(String)
    buscaObjetoById nomeArquivo objetoId = buscaObjetoByAtributo nomeArquivo "id" (show objetoId ++ ",")

    {- 
    Esta função retorna o formato em string de um objeto identificado pelo seu atributo. 
    Caso não seja encontrado o objeto, uma string vazia é retornada.
    
    Forma de uso:
        - Você deve usar delimitadores no valor do atributo. Se for uma String ela deve
        estar envolta em aspas, se for um inteiro deve ter uma vírgula ou uma chave indicando
        seu final.
    
    Parametros:
        nomeArquivo = o nome do arquivo no diretório database no qual o objeto se encontra
        atributo = O atributo do objeto que deseja ser buscado
        valorAtributo = O valor do atributo do objeto que deseja ser buscado
    -}
    buscaObjetoByAtributo :: String -> String -> String -> IO(String)
    buscaObjetoByAtributo nomeArquivo atributo valorAtributo = do
        conteudoEmLista <- fileToStringArray nomeArquivo
        buscaObjetoByAtributoRecursivo conteudoEmLista atributo valorAtributo

    {-
    Esta função trabalha em conjunto com buscaObjetoByAtributo de forma recursiva, buscando o 
    objeto numa lista de todas as linhas do arquivo.
    Caso não seja encontrado o objeto, uma string vazia é retornada.
    Parametros:
        (objetoAtual:objetosRestantes) = é o array de string que representa o objeto, tendo como 
        primeiro elemento o objetoAtual e tendo como os próximos objetos o array ObjetosRestantes.
        atributo = O atributo do objeto que deseja ser buscado
        valorAtributo = O valor do atributo do objeto que deseja ser buscado
    -}
    buscaObjetoByAtributoRecursivo :: [String] -> String -> String -> IO(String)
    buscaObjetoByAtributoRecursivo [] _ _ = return ""
    buscaObjetoByAtributoRecursivo (objetoAtual:objetosRestantes) atributo valorAtributo =
        if (atributo ++ " = " ++ valorAtributo) `T.isInfixOf` objetoAtual
            then return objetoAtual
            else buscaObjetoByAtributoRecursivo objetosRestantes atributo valorAtributo
 
    {-
    Checa se um objeto existe na database, retornando True, caso exista, e False, caso contrário.
    Parametros:
        nomeArquivo = o nome do arquivo no diretório database no qual o objeto se encontra
        idObjeto = O id do objeto que deseja ser buscado
    -}
    checaExistenciaById :: String -> Int -> IO(Bool)
    checaExistenciaById nomeArquivo idObjeto = do
        existeObjeto <- buscaObjetoById nomeArquivo idObjeto
        return (existeObjeto /= "")

    {-
    Checa se um objeto existe na database, retornando True, caso exista, e False, caso contrário.
    Parametros:
        nomeArquivo = o nome do arquivo no diretório database no qual o objeto se encontra
        atributo = O atributo do objeto que deseja ser buscado
        valorAtributo = O valor do atributo do objeto que deseja ser buscado
    -}
    checaExistenciaByAtributo :: String -> String -> String -> IO(Bool)
    checaExistenciaByAtributo nomeArquivo atributo valorAtributo = do
        existeObjeto <- buscaObjetoByAtributo nomeArquivo atributo valorAtributo
        return (existeObjeto /= "")

    {- 
    Esta função adiciona uma linha no arquivo. Caso o arquivo já possua uma linha,
    a funcao irá adicionar na linha seguinte
    Parametros:
        conteudo = Precisa ser no formato string de um objeto 
        nomeArquivo = o nome do arquivo no diretório database
    -}
    adicionaLinha :: String -> String -> IO()
    adicionaLinha nomeArquivo conteudo = do
        let mensagem = conteudo ++ "\n"
        appendFile ("database/" ++ nomeArquivo ++ ".txt") mensagem

    {-
    Esta função busca um valor de id atualizado para uma nova linha da database, sendo esse valor o id da ultima linha somado de 1.
    Caso não haja nenhuma linha presente, será retornado o valor "1".
    Parametros:
        nomeArquivo: o nome do arquivo a ser buscado um novo id no diretório database
    -}
    buscaNovoId :: String -> IO(String)
    buscaNovoId nomeArquivo = do
        conteudoEmLista <- fileToStringArray nomeArquivo
        if (conteudoEmLista == [])
            then return "1"
            else do
                let ultimaLinhaEmLista = P.words (last conteudoEmLista)
                let ultimoId = read (P.take ((P.length (ultimaLinhaEmLista!!3))-1) (ultimaLinhaEmLista!!3)) :: Int
                return (show (ultimoId+1))

    {-
    Essa função atualiza uma linha do arquivo com base no seu id
    Parametros:
        nomeArquivo = o nome do arquivo no diretório database
        id = id da linha 
        novaLinha = conteudo da nova linha a ser atualizada
    -}
    atualizaLinhaById :: String -> String -> String -> IO()
    atualizaLinhaById nomeArquivo id novaLinha = do
        let path = "database/" ++ nomeArquivo ++ ".txt"
        conteudoArquivo <- fileToStringArray nomeArquivo
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
        if ("id = " ++ id ++ ",") `T.isInfixOf` linhaAtual
            then do
                if novaLinha == ""
                    then do
                        atualizaLista linhasRestantes id novaLinha arquivo
                else do
                    hPutStrLn arquivo novaLinha
                    atualizaLista linhasRestantes id novaLinha arquivo
            else do
                hPutStrLn arquivo linhaAtual
                atualizaLista linhasRestantes id novaLinha arquivo

    {-
    Remove uma linha com base no seu id.
    Parametros:
        nomeArquivo = o nome do arquivo no diretório database
        id = id da linha 
    -}
    removeLinha :: String -> String -> IO ()
    removeLinha nomeArquivo id = do
        let path = "database/" ++ nomeArquivo ++ ".txt"
        conteudoArquivo <- fileToStringArray nomeArquivo
        arquivo <- openFile path WriteMode
        hPutStr arquivo ""
        atualizaLista conteudoArquivo id "" arquivo
        hFlush arquivo
        hClose arquivo

    {-
    Adiciona aspas a um String.
    Útil para comparar atributos String na database.
    Parametros:
        texto = texto a ser envolto em aspas
    -}
    adicionaAspas :: String -> String 
    adicionaAspas texto = "\"" ++ texto ++ "\""
