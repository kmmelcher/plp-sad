module Src.Model.Ticket where
    import Src.Model.Mensagem (Mensagem)
    data Ticket = Ticket {
        id :: Integer,
        mensagens :: [Integer],
        status :: String,
        autor :: String
    }
