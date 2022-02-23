module Src.Model.Monitor where
    import Src.Model.Aluno (Aluno)
    import Src.Model.Disciplina (Disciplina)
    data Monitor = Monitor {
        id :: Integer,
        aluno :: Integer,
        disciplina :: String,
        horarios :: String
    }
