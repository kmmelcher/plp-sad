module Model.Disciplina where
    data Disciplina = Disciplina {
        id :: Int,
        sigla :: String,
        nome :: String
    } deriving (Show, Read)
