-- Relatorios.hs
-- Analise pura de logs: sem IO

module Relatorios where

import Data.List (group, sort, maximumBy)
import Data.Ord (comparing)
import Tipos

-- logsDeErro: filtra apenas logs com falha
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter ehFalha
  where
    ehFalha log =
      case status log of
        Sucesso -> False
        Falha _ -> True

-- historicoPorItem: filtra logs que contem o id informado nos detalhes
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem idBuscado =
  filter (\log -> ("id=" ++ idBuscado) `elem` tokens (detalhes log))
  where
    tokens = words

-- itemMaisMovimentado: conta quantas vezes cada id aparece nos logs
-- e retorna o id mais frequente (se existir)
itemMaisMovimentado :: [LogEntry] -> Maybe String
itemMaisMovimentado listaLogs =
  case idsLista of
    []       -> Nothing
    _        -> Just (modo idsLista)
  where
    idsLista = concatMap extrairId listaLogs

    extrairId log =
      [ drop 3 tok
      | tok <- words (detalhes log)
      , take 3 tok == "id="
      ]

    modo listaIds =
      head . maximumBy (comparing length) . group . sort $ listaIds
