module Model.Monitor where
    import Model.Aluno (Aluno)
    import Model.Disciplina (Disciplina)
    data Monitor = Monitor {
        id :: Integer,
        aluno :: Aluno,
        disciplina :: String,
        horarios :: String
    }
