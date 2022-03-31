:- include("../util/jsonFunctions.pl").

getAluno(Id, Aluno):-
    checaExistencia("alunos", Id), 
    getObjetoByID("alunos", Id, Aluno).

vinculaAlunoDisciplina(Id, Disciplina):-

    write("Insira a matricula do aluno (digite 0 para voltar ao seu menu)"),
    read(Id),
%     Id =\= 0,
%     (getAluno(Id, Aluno) ->  
%     (ehMonitorDaDisciplina(Id) -> writeln("Este aluno é monitor de sua disciplina!"); 
%     ))


%         if matricula == 0 then return () else do
%             ehAluno <- checaExistenciaById "Alunos" matricula
%             ehMonitorDaDisciplina <- analisaAlunoComoMonitor matricula disciplina
%             if ehMonitorDaDisciplina then putStrLn "Este aluno é monitor de sua disciplina!"
%             else if ehAluno then do
%                     aluno <- getAluno matricula
%                     if disciplina `elem` A.disciplinas aluno then putStrLn "Este aluno já está na sua diciplina!"
%                     else do
%                         let alunoAtualizado = Aluno (A.id aluno) (nome aluno) (disciplina : disciplinas aluno) (A.senha aluno)
%                         atualizaLinhaById "Alunos" (show matricula) (show alunoAtualizado)
%                         putStrLn "Este aluno já está presente no sistema. Cadastro de aluno na disciplina realizado!"
%             else do
%                 putStrLn "Este aluno não está cadastrado no SAD. Por favor, informe seu nome:"
%                 nome <- getLine
%                 let aluno = Aluno matricula nome [disciplina] (encripta "Aluno" nome)
%                 adicionaLinha "Alunos" $ show aluno
%                 putStrLn "Aluno cadastrado com sucesso e incluso na disciplina.\n"
% vinculaAlunoDisciplina(Id, Disciplina).