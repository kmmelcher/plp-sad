module Model.Aluno where
    import Model.Disciplina (Disciplina)
    data Aluno = Aluno {
        id :: Integer,
        nome :: String,
        disciplinas:: [String]
    } deriving (Show, Read)
