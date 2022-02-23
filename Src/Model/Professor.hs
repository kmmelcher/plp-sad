module Src.Model.Professor where
    import Src.Model.Disciplina (Disciplina)
    import GHC.Read (Read)
    data Professor = Professor {
        id :: String,
        nome :: String,
        disciplinas:: [String]
    } deriving (Show, Read)