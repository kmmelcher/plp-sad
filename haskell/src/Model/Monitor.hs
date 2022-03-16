module Model.Monitor where
    import Model.Aluno (Aluno)
    import Model.Disciplina (Disciplina)
    data Monitor = Monitor {
        id :: Int,
        disciplina :: String,
        horarios :: String
    } deriving (Read, Show)
