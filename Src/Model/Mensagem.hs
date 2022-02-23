module Src.Model.Mensagem where
    data Mensagem = Mensagem {
        id :: Integer,
        autor :: String,
        conteudo :: String,
        horario :: String
    }