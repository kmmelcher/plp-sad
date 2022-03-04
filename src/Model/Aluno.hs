module Model.Aluno where
    data Aluno = Aluno {
        id :: Int,
        nome :: String,
        disciplinas:: [String],
        senha :: String
    } deriving (Show, Read)
