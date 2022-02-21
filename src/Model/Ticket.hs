module Model.Ticket where
    import Model.Mensagem (Mensagem)
    data Ticket = Ticket {
        id :: Integer,
        mensagens :: [Mensagem],
        status :: String,
        autor :: String
    }
