module Model.Mensagem where
    import Data.Time (UTCTime)
    data Mensagem = Mensagem {
        id :: Int,
        autor :: Int,
        conteudo :: String,
        horario :: String
    } deriving (Show, Read)