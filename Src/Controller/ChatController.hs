{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Src.Controller.ChatController where
    import Src.Model.Mensagem as MSG
    import Src.Model.Aluno as A
    import qualified Src.Model.Ticket as T
    import Src.Util.TxtFunctions
    import Data.Time (getCurrentTime, UTCTime)
    import Data.Time.Format
    import Src.Model.Monitor as M
    import Src.Model.Professor as P
    import Src.Controller.AlunoController
    import Src.Controller.ProfessorController
    import Src.Model.Ticket (Ticket)
    import Src.Model.Disciplina
    
    {- 
    Função que retorna um objeto do tipo ticket a partir de um ID fornecido
    Parametros:
        id = O id do ticket desejado
    -}
    getTicket:: Int -> IO T.Ticket
    getTicket id = do
        ticketToString <- getObjetoById "Tickets" id
        return (read ticketToString :: T.Ticket)
    
    {- 
    Função que retorna um objeto do tipo Mensagem a partir de um ID fornecido
    Parametros:
        id = id da mensagem desejada
    -}
    getMensagem:: Int -> IO Mensagem
    getMensagem id = do
        mensagemToString <- getObjetoById "Mensagens" id
        return (read mensagemToString :: Mensagem)

    {- 
    Adiciona um ticket com um aluno como autor
    Forma de uso:
        A função deverá ser chamada com o aluno autor como parametro e as demais entradas nescessárias são fornecidas pelo usuário
    Parametros:
        aluno = O aluno autor do ticket
    -}
    adicionaTicket :: Aluno -> IO()
    adicionaTicket aluno = do
        putStrLn "\nInsira a sigla da disciplina na qual deseja criar o ticket:"
        disciplinaTicket <-  getLine
        if disciplinaTicket `elem` A.disciplinas aluno then do
            putStrLn "Insira um título para sua dúvida:"
            titulo <- getLine
            id <- buscaNovoId "Tickets"
            let ticket = T.Ticket (read id) titulo [] "Em Andamento" (A.id aluno) disciplinaTicket
            adicionaLinha "Tickets" $ show ticket
            putStrLn "\nTicket adicionado com sucesso!"
        else do
            putStrLn "Você não possui está cadastrado nesta disciplina! Tente novamente."
            adicionaTicket aluno
    
    {- 
        Função que adiciona uma mensagem à um ticket existente
    Parametros:
        id = id do Autor da mensagem
        ticketsValidos = Tickets em que esse autor pode mandar mensagens
    -}
    adicionaMensagem :: Int -> [Int] -> IO()
    adicionaMensagem id ticketsValidos = do
        putStrLn "Escolha o ticket no qual deseja inserir a mensagem: "
        idTicket <- readLn
        if ehTicketValido idTicket ticketsValidos
            then do
            putStrLn "Digite a mensagem:"
            conteudo <- getLine
            idMensagem <- buscaNovoId "Mensagens"
            tempo <- getCurrentTime >>= return.formatTime defaultTimeLocale "%D %Hh%M"
            let mensagem = Mensagem (read idMensagem) id conteudo tempo
            insereMensagemNoTicket idTicket (read idMensagem)
            adicionaLinha "Mensagens" $ show mensagem
            putStrLn "Mensagem adicionada com sucesso."
        else do
            putStrLn "Ticket inválido!\n"
            adicionaMensagem id ticketsValidos

    {- 
    Função que avalia se um ticket pertence ao grupo de tickets válidos recebidos
    Parametros:
        ticketId = id do ticket à ser analisado
        ticketsValidos = grupo de tickets válidos 
    -}
    ehTicketValido :: Int -> [Int] -> Bool
    ehTicketValido ticketId ticketsValidos = ticketId `elem` ticketsValidos
    
    {- 
    Retorna os ids de todos os tickets de um aluno
    Parametros:
        matricula = A matricula do aluno
    -}
    -- Não poderia retornar um array de tickets?
    getTicketsAluno :: Int -> IO[Int]
    getTicketsAluno matricula = do
        tickets <- fileToStringArray "Tickets"
        getTicketsAlunoRecursivo tickets matricula

    {- 
    Função auxiliar de getTicketsAluno que retorna os tickets de um aluno
    -}
    getTicketsAlunoRecursivo :: [String] -> Int -> IO [Int]
    getTicketsAlunoRecursivo [] _ = return []
    getTicketsAlunoRecursivo (ticketAtual:ticketsRestantes) matricula = do
        let ticket = (read ticketAtual :: T.Ticket)
        proximos <- getTicketsAlunoRecursivo ticketsRestantes matricula
        if T.autor ticket == matricula then do
            return (T.id ticket : proximos)
            else return proximos

    {- 
    Insere uma mensagem em um Ticket.
    Parametros
        idTicket : ticket no qual voce quer inserir a mensagem.
        idMensagem: Mensagem a ser inserida no Ticket.
    -}
    insereMensagemNoTicket :: Int -> Int -> IO()
    insereMensagemNoTicket idTicket idMensagem = do
        ticket <- getTicket idTicket
        let ticketAtualizado = T.Ticket idTicket (T.titulo ticket) (T.mensagens ticket ++ [idMensagem]) (T.status ticket) (T.autor ticket) (T.disciplina ticket)
        atualizaLinhaById "Tickets" (show idTicket) (show ticketAtualizado)


    {-
    Exibe todos os tickets de uma disciplina.
    Parâmetros:
        nomeDisciplina: Nome da disciplina a ser exibida.
    -}
    exibeTicketsDisciplina :: String -> IO()
    exibeTicketsDisciplina nomeDisciplina = do
        tickets <- getTicketsDisciplina nomeDisciplina
        exibeTickets tickets "nesta disciplina" ("da disciplina: " ++ nomeDisciplina)


    getTodosOsTickets:: IO [String]
    getTodosOsTickets = do
        fileToStringArray "Tickets"
       

    {- 
    Retorna todos os tickets de uma disciplina
    Parametros
        disciplina: qual o nome da disciplina dos tickets que queremos retornar
    -}
    getTicketsDisciplina :: String -> IO[Int]
    getTicketsDisciplina disciplina = do

        tickets <- fileToStringArray "Tickets"
        getTicketsDisciplinaRecursivo tickets disciplina


    {- 
    Função auxiliar de getTicketsDisciplina
    -}
    getTicketsDisciplinaRecursivo :: [String] -> String -> IO[Int]
    getTicketsDisciplinaRecursivo [] _ = return []
    getTicketsDisciplinaRecursivo (ticketAtual:ticketsRestantes) disciplina = do
        let ticket = (read ticketAtual :: T.Ticket)
        if T.disciplina ticket == disciplina
            then do
                proximos <- getTicketsDisciplinaRecursivo ticketsRestantes disciplina
                return (T.id ticket : proximos)
            else getTicketsDisciplinaRecursivo ticketsRestantes disciplina

    {- 
    Mostra no terminal os tickets em andamento dentre os tickets com os ids presentes na entrada
    Parametros:
        tickets = Os tickets que serão analisados
    -}
    exibeTicketsEmAndamento :: [Int] -> IO()
    exibeTicketsEmAndamento tickets = do
        ticketsEmAndamento <- getTicketsEmAndamento tickets
        exibeTickets ticketsEmAndamento "em andamento" "e em andamento"
    
    {- 
    Retorna um array com os ids dos tickets que estão em andamento
    Parametros:
        tickets = grupo de tickets a serem analisados
    -}
    getTicketsEmAndamento :: [Int] -> IO[Int]
    getTicketsEmAndamento tickets = do
        getTicketsEmAndamentoRecursivo tickets

    {- 
    Função auxiliar de getTicketsEmAndamento
    -}
    getTicketsEmAndamentoRecursivo :: [Int] -> IO [Int]
    getTicketsEmAndamentoRecursivo [] = return []
    getTicketsEmAndamentoRecursivo (ticketAtual:ticketsRestantes) = do
        ticket <- getTicket ticketAtual
        if T.status ticket == "Em Andamento" then do
            proximosTickets <- getTicketsEmAndamentoRecursivo ticketsRestantes
            return (T.id ticket : proximosTickets)
            else
                getTicketsEmAndamentoRecursivo ticketsRestantes
    
    {- 
    Retorna um array com os ids dos tickets que estão concluidos
    Parametros:
        tickets = Os tickets a serem analisados
    -}
    getTicketsConcluidos :: [Int] -> IO[Int]
    getTicketsConcluidos tickets = do
        getTicketsConcluidosRecursivo tickets

    {- 
    Função auxiliar de getTicketsConcluidos
    -}
    getTicketsConcluidosRecursivo :: [Int] -> IO [Int]
    getTicketsConcluidosRecursivo [] = return []
    getTicketsConcluidosRecursivo (ticketAtual:ticketsRestantes) = do
        ticket <- getTicket ticketAtual
        if T.status ticket == "Resolvido" then do
            proximosTickets <- getTicketsConcluidosRecursivo ticketsRestantes
            return (T.id ticket : proximosTickets)
            else
                getTicketsConcluidosRecursivo ticketsRestantes


    {-
    Exibe uma lista de tickets.
    Checa se a lista de tickets é vazia, nesse caso é avisado ao usuário.
    -}
    exibeTickets :: [Int] -> String -> String -> IO()
    exibeTickets tickets mensagemSemTicket mensagemComTickets = do
        if null tickets
            then putStrLn ("\nAinda não há tickets " ++ mensagemSemTicket ++ ".\n")
        else do
            putStrLn ("\nEstes são os tickets existentes " ++ mensagemComTickets ++ "\n")
            exibeTicketsRecursivo tickets

    {-
    Itera recursivamente sobre uma lista de tickets exibindo cada um.
    -}
    exibeTicketsRecursivo :: [Int] -> IO()
    exibeTicketsRecursivo [] = do
        putStr "\n"
        return ()
    exibeTicketsRecursivo (idTicketAtual:idsTicketsRestantes) = do
        ticket <- getTicket idTicketAtual
        putStrLn (show (T.id ticket) ++ "- " ++ T.titulo ticket ++ " (" ++ T.status ticket ++ ")")  
        exibeTicketsRecursivo idsTicketsRestantes

    {- 
    Checa se um ticket está em andamento 
    Parametros:
        id = Id do ticket a ser analisado
    -}
    checaIdDeTicketEmAndamento :: Int -> IO Bool
    checaIdDeTicketEmAndamento id = do
        ticket <- getTicket id
        return (T.status ticket == "Em Andamento")

    {- 
    Atualiza o status de um ticket para resolvido
    Parametros:
        id = Id do ticket a ser atualizado
    -}
    atualizaTicketStatus :: Int -> IO()
    atualizaTicketStatus id = do
        ticket <- getTicket id
        let novoTicket = T.Ticket (T.id ticket) (T.titulo ticket) (T.mensagens ticket) "Resolvido" (T.autor ticket) (T.disciplina ticket)
        atualizaLinhaById "Tickets" (show id) (show novoTicket)

    {- 
    Função com as interçaões com usuario nescessárias para a resolução de um ticket
    Parametros:
        aluno = O aluno que possui o ticket que será resolvido
    -}
    resolveTicket :: Aluno -> IO()
    resolveTicket aluno = do
        ticketsAluno <- getTicketsAluno (A.id aluno)
        putStrLn "Estes são os seus tickets com status \"Em andamento\"\n"
        exibeTicketsEmAndamento ticketsAluno
        putStrLn "\ninsira o id do ticket que deseja marcar como concluído"
        input <- getLine
        let id = read input :: Int
        idValido <- checaIdDeTicketEmAndamento id
        if idValido then do
            atualizaTicketStatus id
            putStrLn "Ticket alterado com sucesso.\n"
             else do
            putStrLn "Insira um id válido!"
            resolveTicket aluno

    {- 
    Função para um monitor adicionar uma mensagem a um ticket atraves de entradas do usuário
    Parametros:
        monitor = O monitor que irá enviar a mensagem
    -}
    adicionaMensagemMonitor :: Monitor -> IO()
    adicionaMensagemMonitor monitor = do
        tickets <- getTicketsDisciplina (M.disciplina monitor)
        exibeTicketsEmAndamento tickets
        ticketsEmAndamento <- getTicketsEmAndamento tickets
        if null tickets
            then return ()
        else do
            adicionaMensagem (M.id monitor) ticketsEmAndamento

    {- 
    Fução para um aluno inserir uma mensagem em um dos seus tickets atravez de entradas do usuario
    Parametros:
        aluno = O aluno que irá enviar a mensagem
    -}
    adicionaMensagemAluno :: Aluno -> IO()
    adicionaMensagemAluno aluno = do
        tickets <- getTicketsAluno (A.id aluno)
        ticketsEmAndamento <- getTicketsEmAndamento tickets
        if null ticketsEmAndamento then exibeTicketsEmAndamento tickets else do
            exibeTicketsEmAndamento tickets
            adicionaMensagem (A.id aluno) ticketsEmAndamento

    {- 
    Função que permite um aluno excluir um de seus tickets atravez de entradas do usuário
    Parametros:
        aluno = o aluno que irá excluir o ticket
    -}
    excluirTicket :: Aluno -> IO()
    excluirTicket aluno = do
        ticketsIds <- getTicketsAluno (A.id aluno)
        exibeTickets ticketsIds "para exclusão" "existentes"
        putStrLn "Escolha entre os seus Tickets qual será excluido: "
        input <- getLine
        if read input `elem` ticketsIds then do
            removeLinha "Tickets" input
            putStrLn "Ticket removido com sucesso."
            else do
                putStrLn "Id invalido!"
                excluirTicket aluno

    {- 
    Função que mostra todos os tickets de um aluno e depois mostra as mensgens de um ticket especifico
    Parametros:
        aluno = O aluno dono dos tickets
    -}
    leTicketsDoAluno :: Aluno -> IO()
    leTicketsDoAluno aluno = do
        ticketsAluno <- getTicketsAluno (A.id aluno)
        if null ticketsAluno then exibeTickets ticketsAluno "de sua autoria" "criados por você" else do
            exibeTickets ticketsAluno "de sua autoria" "criados por você"
            putStrLn "Insira o id do ticket que deseja ler as mensagens: "
            idTicket <- readLn
            if idTicket `elem` ticketsAluno then exibeMensagensDeTicket idTicket else do
                putStrLn "Id de ticket invalido!"
                leTicketsDoAluno aluno

    {- 
    Função para exibir as mensgens de um ticket em forma de chat
    Parametros:
        idTicket = o id do ticket que contem as mensagens
    -}
    exibeMensagensDeTicket :: Int -> IO()
    exibeMensagensDeTicket idTicket = do
        mensagens <- getMensagensDoTicket idTicket
        ticket <- getTicket idTicket
        exibeMensagensDoTicketRecursivo mensagens (T.disciplina ticket)

    {- 
    Função auxiliar de exibeMensagensDeTicket
    -}
    exibeMensagensDoTicketRecursivo :: [IO Mensagem] -> String -> IO()
    exibeMensagensDoTicketRecursivo [] x = return ()
    exibeMensagensDoTicketRecursivo (mensagemAtualIO:mensagensRestantesIO) disciplina = do
        mensagemAtual <- mensagemAtualIO
        exibeMensagem mensagemAtual disciplina
        exibeMensagensDoTicketRecursivo mensagensRestantesIO disciplina

    {- 
    Retorna os Ids de todas as mensgens de um ticket
    Parametros:
        idTicket = id do ticket que contem as mensagens
    -}
    getMensagensDoTicket :: Int -> IO[IO Mensagem]
    getMensagensDoTicket idTicket = do
        ticket <- getTicket idTicket
        let mensagensTicket = T.mensagens ticket
        return(map getMensagem mensagensTicket)

    {- 
    Mostra o texto de uma mensagem juntamente com o autor e o horario
    Parametros:
        mensagem = Mensagem que será mostrada
        disciplina = a disciplina do ticket que contem a mensagem
    -}
    exibeMensagem :: Mensagem -> String ->IO()
    exibeMensagem mensagem disciplina = do
        ehProf <- checaExistenciaById "Professores" (autor mensagem)
        ehMonitor <- checaExistenciaById "Monitores" (autor mensagem)
        monitorStr <- getObjetoById "Monitores" (autor mensagem)
        let monitor = read monitorStr :: Monitor

        if ehProf then do
            professor <- getProfessor (autor mensagem)
            putStrLn ("[" ++ horario mensagem ++ "] " ++ "(PROFESSOR) " ++ P.nome professor ++ " - " ++ conteudo mensagem)
        else do
            aluno <- getAluno (autor mensagem)
            if ehMonitor && (M.disciplina monitor == disciplina) then
                putStrLn ("[" ++ horario mensagem ++ "] " ++ "(MONITOR) " ++ A.nome aluno ++ " - " ++ conteudo mensagem)
            else putStrLn ("[" ++ horario mensagem ++ "] " ++ "(ALUNO) " ++ A.nome aluno ++ " - " ++ conteudo mensagem)

    {- 
    Mostra as mensagens de um dos tickets do aluno atravez de entradas do usuario
    Parametros:
        aluno = Aluno que possui os tickets
    -}
    leTicketsDaDisciplinaAluno :: Aluno -> IO()
    leTicketsDaDisciplinaAluno aluno = do
        putStrLn "\nInforme a sigla da disciplina na qual deseja visualizar os tickets:"
        disciplina <- getLine
        if disciplina `elem` A.disciplinas aluno then do
            exibeMensagensDisciplina disciplina
        else do
            putStrLn "\nEsta sigla não é valida ou é referente a uma disciplina na qual você não está matrículado!\n"
            leTicketsDaDisciplinaAluno aluno

    {- 
    Mostra as mensagens de uma disciplina expecifica
    Parametros:
        disciplina = sigla da disciplina desejada
    -}
    exibeMensagensDisciplina :: String -> IO()
    exibeMensagensDisciplina disciplina = do
        tickets <- getTicketsDisciplina disciplina
        if null tickets then exibeTicketsDisciplina disciplina else do
            exibeTicketsDisciplina disciplina
            putStrLn "Insira o id do Ticket que deseja visualizar ou 0 para sair"
            idTicket <- readLn
            if idTicket == 0 then return ()
            else do
                ticketsDisciplina <- getTicketsDisciplina disciplina
                if idTicket `elem` ticketsDisciplina then exibeMensagensDeTicket idTicket else do
                    putStrLn "Insira um valor válido!"
                    exibeMensagensDisciplina disciplina

    {- 
    Mostra as mensgens de um dos tickets de uma das disciplinas do professor atravez de entradas do usuario
    Parametros:
        professor = professor a ser analisado
    -}
    lerTicketsDisciplinaProfessor :: Professor -> IO()
    lerTicketsDisciplinaProfessor professor = do
        if length (P.disciplinas professor) > 1 then do
            putStrLn "\nInsira a sigla da disciplina na qual você deseja visualizar os tickets:"
            disciplina <- getLine
            if ehDisciplinaDoProfessor professor disciplina
                then exibeMensagensDisciplina disciplina
                else do
                    putStrLn "\nDisciplina invalida!"
                    lerTicketsDisciplinaProfessor professor
        else do
               exibeMensagensDisciplina (head (P.disciplinas professor))