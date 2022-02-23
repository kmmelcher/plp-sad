import Util.TxtFunctions
import Model.Aluno

createAluno :: IO()
createAluno = do
    print "Insira o nome do aluno: "
    nome <- getLine
    print "Insira a matricula do aluno"
    matricula <- readLn
    print "Insira as disciplinas do aluno"
    disciplinas <- readLn
    let aluno = Aluno matricula nome disciplinas
    adicionaLinha "../database/alunos.txt" $ show aluno
