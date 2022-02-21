import Util.CsvFunctions


main :: IO()
main = do
    print "Arquivo"
    arquivo <- getLine
    print "Insira os dados:"
    dados <- getLine
    adicionaLinhaCsv arquivo dados
