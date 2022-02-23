module Model.Aluno where
    import Model.Disciplina (Disciplina)
    data Aluno = Aluno {
        matricula :: Integer,
        nome :: String,
        disciplinas:: [String]
    }
