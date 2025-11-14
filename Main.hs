-- Main.hs
-- ponto de entrada, menu e IO (entrada/saida + arquivos)

module Main where

import qualified Data.Map as Map
import Data.Time (getCurrentTime)
import System.IO (hFlush, stdout)

import Tipos
import Logica
import Relatorios
import Persistencia

-- inicia o programa carregando o inventario e chamando o loop
main :: IO ()
main = do
  putStrLn "Inventario iniciado"
  inv <- carregarInventario
  loop inv

-- loop principal: le comando do usuario e chama o fluxo certo
loop :: Inventario -> IO ()
loop inv = do
  putStrLn "\nComandos: add | remove | update | list | report | exit"
  putStr "> "
  hFlush stdout
  cmd <- getLine
  case cmd of
    "add"    -> fluxoAdd inv
    "remove" -> fluxoRemove inv
    "update" -> fluxoUpdate inv
    "list"   -> fluxoList inv
    "report" -> fluxoReport inv
    "exit"   -> putStrLn "Encerrando..."
    _        -> do
                  putStrLn "Comando invalido."
                  loop inv

-- fluxo de adicionar item (le dados, chama logica e atualiza arquivo)
fluxoAdd :: Inventario -> IO ()
fluxoAdd inv = do
  putStr "id: "   >> hFlush stdout
  idItem <- getLine
  putStr "nome: " >> hFlush stdout
  nomeItem <- getLine
  putStr "qtd: "  >> hFlush stdout
  qntStr <- getLine
  putStr "cat: "  >> hFlush stdout
  cat <- getLine

  let qnt  = read qntStr
      item = Item idItem nomeItem qnt cat

  tmp <- getCurrentTime
  case addItem tmp item inv of
    Left erro -> do
      appendLog (LogEntry tmp Add ("id=" ++ idItem) (Falha erro))
      putStrLn ("Erro: " ++ erro)
      loop inv
    Right (invNovo, logGerado) -> do
      salvarInventario invNovo
      appendLog logGerado
      putStrLn "Item adicionado."
      loop invNovo

-- fluxo de remover quantidade de um item
fluxoRemove :: Inventario -> IO ()
fluxoRemove inv = do
  putStr "id: "  >> hFlush stdout
  idItem <- getLine
  putStr "qtd: " >> hFlush stdout
  qntStr <- getLine
  let qnt = read qntStr

  tmp <- getCurrentTime
  case removeItem tmp idItem qnt inv of
    Left erro -> do
      appendLog (LogEntry tmp Remove ("id=" ++ idItem ++ " q=" ++ show qnt) (Falha erro))
      putStrLn ("Erro: " ++ erro)
      loop inv
    Right (invNovo, logGerado) -> do
      salvarInventario invNovo
      appendLog logGerado
      putStrLn "Remocao ok."
      loop invNovo

-- fluxo de atualizar a quantidade direta de um item
fluxoUpdate :: Inventario -> IO ()
fluxoUpdate inv = do
  putStr "id: "  >> hFlush stdout
  idItem <- getLine
  putStr "qtd: " >> hFlush stdout
  qntStr <- getLine
  let qnt = read qntStr

  tmp <- getCurrentTime
  case updateQty tmp idItem qnt inv of
    Left erro -> do
      appendLog (LogEntry tmp Update ("id=" ++ idItem ++ " q=" ++ show qnt) (Falha erro))
      putStrLn ("Erro: " ++ erro)
      loop inv
    Right (invNovo, logGerado) -> do
      salvarInventario invNovo
      appendLog logGerado
      putStrLn "Atualizacao ok."
      loop invNovo

-- fluxo de listar todos os itens salvos no inventario
fluxoList :: Inventario -> IO ()
fluxoList inv = do
  mapM_ printItem (Map.elems inv)
  tmp <- getCurrentTime
  appendLog (LogEntry tmp List "list" Sucesso)
  loop inv

-- fluxo de relatorio: conta logs, erros e item mais movimentado
fluxoReport :: Inventario -> IO ()
fluxoReport inv = do
  listaLogs <- carregarLogs
  putStrLn ("total logs: " ++ show (length listaLogs))
  putStrLn ("erros: " ++ show (length (logsDeErro listaLogs)))
  case itemMaisMovimentado listaLogs of
    Nothing     -> putStrLn "nenhum item movimentado"
    Just idItem -> putStrLn ("item mais movimentado: " ++ idItem)
  tmp <- getCurrentTime
  appendLog (LogEntry tmp Report "report" Sucesso)
  loop inv

-- imprime um item em uma linha simples
printItem :: Item -> IO ()
printItem itemAtual =
  putStrLn (
    "id="  ++ itemID itemAtual ++
    " nome=" ++ nome itemAtual ++
    " q="    ++ show (quantidade itemAtual) ++
    " cat="  ++ categoria itemAtual
  )
