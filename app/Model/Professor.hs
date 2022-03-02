module Model.Professor where
    data Professor = Professor {
        id :: Int,
        nome :: String,
        disciplinas:: [String]
    } deriving (Show, Read)