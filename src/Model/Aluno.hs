module Model.Aluno where
    data Aluno = Aluno {
        id :: Int,
        nome :: String,
        disciplinas:: [String]
    } deriving (Show, Read)
