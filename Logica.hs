-- Logica.hs
-- funcoes puras da logica do inventario

module Logica where

import qualified Data.Map as Map
import Data.Time (UTCTime)
import Tipos

-- addItem: tenta adicionar um novo item no inventario
-- se o id ja existir, retorna erro (Left)
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem tmp item inv =
  if Map.member (itemID item) inv
    then Left "Item ja existe"
    else
      let invNovo   = Map.insert (itemID item) item inv
          logGerado = LogEntry tmp Add ("id=" ++ itemID item) Sucesso
      in Right (invNovo, logGerado)

-- removeItem: tenta remover uma quantidade de um item
-- valida se o item existe, se a quantidade e valida, e se ha estoque suficiente
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem tmp idItem qnt inv =
  case Map.lookup idItem inv of
    Nothing -> Left "Item nao encontrado"
    Just itemAtual ->
      if qnt <= 0 then Left "Quantidade invalida"
      else if quantidade itemAtual < qnt then Left "Estoque insuficiente"
      else
        let novoQnt = quantidade itemAtual - qnt
            invNovo =
              if novoQnt == 0
                then Map.delete idItem inv
                else Map.insert idItem (itemAtual { quantidade = novoQnt }) inv
            logGerado = LogEntry tmp Remove ("id=" ++ idItem ++ " q=" ++ show qnt) Sucesso
        in Right (invNovo, logGerado)

-- updateQty: ajusta a quantidade de um item diretamente
-- se nao existir, erro; se quantidade negativa, erro
updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty tmp idItem qnt inv =
  case Map.lookup idItem inv of
    Nothing -> Left "Item nao encontrado"
    Just itemAtual ->
      if qnt < 0 then Left "Quantidade invalida"
      else
        let invNovo =
              if qnt == 0
                then Map.delete idItem inv
                else Map.insert idItem (itemAtual { quantidade = qnt }) inv
            logGerado = LogEntry tmp Update ("id=" ++ idItem ++ " q=" ++ show qnt) Sucesso
        in Right (invNovo, logGerado)
