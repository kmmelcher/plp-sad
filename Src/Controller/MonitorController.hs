module Src.Controller.MonitorController where
    import Src.Model.Monitor
    import Src.Util.TxtFunctions

    adicionaMonitor :: IO()
    adicionaMonitor = do
        -- Devemos analisar primeiro se há a instancia dessa matrícula em aluno!
        putStrLn "Essa funcionalidade ainda está em construção, volte mais tarde.\n"