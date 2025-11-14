-- IO: Input/Output (auditoria e inventario)--

module Persistencia where

import System.IO.Error (catchIOError)
import qualified Data.Map as Map
import Tipos

-- defini os arquivos usados na persistencia
caminhoInventario :: FilePath
caminhoInventario = "Inventario.dat"

caminhoLog :: FilePath
caminhoLog = "Auditoria.log"

-- carrega o inventario do arquivo .dat
-- se o arquivo nao existir, comeca com inventario vazio
carregarInventario :: IO Inventario
carregarInventario =
  catchIOError
    (do txt <- readFile caminhoInventario
        return (read txt))
    (\_ -> return Map.empty)

-- salva o inventario atual no .dat (sobrescreve)
salvarInventario :: Inventario -> IO ()
salvarInventario inv =
  writeFile caminhoInventario (show inv)

-- carrega todos os logs do arquivo .log
-- se nao existir, volta lista vazia
carregarLogs :: IO [LogEntry]
carregarLogs =
  catchIOError
    (do txt <- readFile caminhoLog
        return (map read (lines txt)))
    (\_ -> return [])

-- transforma o LogEntry em texto e adiciona no final do .log
appendLog :: LogEntry -> IO ()
appendLog log =
  appendFile caminhoLog (show log ++ "\n")
