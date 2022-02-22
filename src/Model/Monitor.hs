module Model.Monitor where
    import Model.Aluno (Aluno)
    import Model.Disciplina (Disciplina)
    data Monitor = Monitor {
        id :: Integer,
        aluno :: Integer,
        disciplina :: String,
        horarios :: String
    }
