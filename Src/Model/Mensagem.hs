module Src.Model.Mensagem where
    import Data.Time (UTCTime)
    data Mensagem = Mensagem {
        id :: Int,
        autor :: String,
        conteudo :: String,
        horario :: String
    } deriving (Show, Read)