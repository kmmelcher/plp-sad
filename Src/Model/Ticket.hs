module Model.Ticket where
    import Model.Mensagem (Mensagem)
    data Ticket = Ticket {
        id :: Integer,
        mensagens :: [Integer],
        status :: String,
        autor :: String
    }
