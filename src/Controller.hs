import Util.TxtFunctions
import Model.Professor

addProfessor :: IO()
addProfessor = do
        putStrLn "Insira o id do professor: "
        idStr <- getLine
        let id = read(idStr) :: Integer
        putStrLn "Insira o nome do professor: "
        nome <- getLine
        putStrLn "Insira o nome das disciplinas do professor: "
        listaDisciplinasStr <- getLine
        let disciplinas = read(listaDisciplinasStr) :: [String]
        let prof = Professor id nome disciplinas
        let profToString = show (prof)
        adicionaLinha "../database/Professores.txt" profToString