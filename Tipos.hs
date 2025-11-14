--Somente funcoes puras, sem logica e Regras de negocio--

module Tipos where

import qualified Data.Map as Map
import Data.Time (UTCTime)

-- define o item do inventario
data Item = Item
  { itemID     :: String
  , nome       :: String
  , quantidade :: Int
  , categoria  :: String
  } deriving (Show, Read, Eq)

-- inventario e um Map com id do item e o Item
type Inventario = Map.Map String Item

-- AcaoLog: quais acoes podem ser registradas no log
-- QueryFail: falha generica de consulta/entrada
data AcaoLog
  = Add
  | Remove
  | Update
  | List
  | Report
  | QueryFail
  deriving (Show, Read, Eq)

-- StatusLog: resultado da acao
data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read, Eq)

-- LogEntry: gera uma linha de auditoria
data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao      :: AcaoLog
  , detalhes  :: String
  , status    :: StatusLog
  } deriving (Show, Read, Eq)

-- ResultadoOperacao: novo estado do inventario + log gerado
type ResultadoOperacao = (Inventario, LogEntry)
