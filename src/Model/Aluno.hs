module Src.Model.Aluno where
    import Src.Model.Disciplina (Disciplina)
    data Aluno = Aluno {
        id :: Integer,
        nome :: String,
        disciplinas:: [String]
    } deriving (Show, Read)
