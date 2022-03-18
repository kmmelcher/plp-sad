module Model.Ticket where
    import Model.Mensagem (Mensagem)
    data Ticket = Ticket {
        id :: Int,
        titulo :: String,
        mensagens :: [Int],
        status :: String,
        autor :: Int,
        disciplina :: String
    } deriving (Show, Read)
