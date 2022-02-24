module Src.Model.Monitor where
    import Src.Model.Aluno (Aluno)
    import Src.Model.Disciplina (Disciplina)
    data Monitor = Monitor {
        id :: Int,
        aluno :: Aluno,
        disciplina :: String,
        horarios :: String
    }
