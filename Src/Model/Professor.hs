module Src.Model.Professor where
    import Src.Model.Disciplina (Disciplina)
    import GHC.Read (Read)
    data Professor = Professor {
        id :: Integer,
        nome :: String,
        disciplinas:: [String]
    } deriving (Show, Read)