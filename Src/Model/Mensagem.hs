module Src.Model.Mensagem where
    import Data.Time (UTCTime)
    data Mensagem = Mensagem {
        id :: Int,
        autor :: String,
        conteudo :: String,
        horario :: String,
        ticketId :: Int
    } deriving (Show, Read)