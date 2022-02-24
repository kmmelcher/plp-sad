module Src.Model.Ticket where
    import Src.Model.Mensagem (Mensagem)
    data Ticket = Ticket {
        id :: Int,
        mensagens :: [Int],
        status :: String,
        autor :: Int,
        disciplina :: String
    } deriving (Show, Read)
