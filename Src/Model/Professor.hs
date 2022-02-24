module Src.Model.Professor where
    import GHC.Read (Read)
    data Professor = Professor {
        id :: Int,
        nome :: String,
        disciplinas:: [String]
    } deriving (Show, Read)