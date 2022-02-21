module Model.Professor where
    import Model.Disciplina (Disciplina)
    data Professor = Professor {
        id :: Integer,
        nome :: String,
        disciplinas:: [Disciplina]
    }