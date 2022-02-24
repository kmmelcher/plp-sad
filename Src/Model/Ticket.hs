module Src.Model.Ticket where
    import Src.Model.Mensagem (Mensagem)
    data Ticket = Ticket {
        id :: Integer,
        mensagens :: [Mensagem],
        status :: String,
        autor :: String,
        disciplina :: String    --TODO Trazer objeto monitor para cada ticket
    } deriving (Show, Read)
