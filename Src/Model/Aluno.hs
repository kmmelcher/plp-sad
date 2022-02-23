module Src.Model.Aluno where
    import Src.Model.Disciplina (Disciplina)
    data Aluno = Aluno {
        id :: String,
        nome :: String,
        disciplinas:: [String]
    } deriving (Show, Read)
