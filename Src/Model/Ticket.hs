module Src.Model.Ticket where
    import Src.Model.Mensagem (Mensagem)
    data Ticket = Ticket {
        id :: Int,
        mensagens :: [Integer],
        status :: String,
        autor :: String,
        disciplina :: String
    } deriving (Show, Read)
