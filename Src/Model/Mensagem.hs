module Src.Model.Mensagem where
    data Mensagem = Mensagem {
        id :: String,
        autor :: String,
        conteudo :: String,
        horario :: String
    }