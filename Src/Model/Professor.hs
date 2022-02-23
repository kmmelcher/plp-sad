module Src.Model.Professor where
    import Src.Model.Disciplina (Disciplina)
    data Professor = Professor {
        id :: Integer,
        nome :: String,
        disciplinas:: [String]
    }