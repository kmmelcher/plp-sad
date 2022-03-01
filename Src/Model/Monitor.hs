module Src.Model.Monitor where
    import Src.Model.Aluno (Aluno)
    import Src.Model.Disciplina (Disciplina)
    data Monitor = Monitor {
        id :: Int,
        disciplina :: String,
        horarios :: String
    } deriving (Read, Show)
