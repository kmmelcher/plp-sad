module Model.Aluno where
    import Model.Disciplina (Disciplina)
    data Aluno = Aluno {
        id :: Integer,
        matricula :: Integer,
        nome :: String,
        disciplinas:: [Disciplina]
    }
