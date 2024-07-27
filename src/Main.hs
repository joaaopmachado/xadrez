module Main where

import Tabuleiro
import ProcessarMovimento (processarMovimento)
import Check (verificarXequeAposMovimento)

estadoInicial :: (Tabuleiro, Cor)
estadoInicial = (tabuleiroInicial, Branca)

alternaJogador :: Cor -> Cor
alternaJogador Branca = Preta
alternaJogador Preta = Branca

loopJogo :: (Tabuleiro, Cor) -> IO ()
loopJogo estado@(tab, cor) = do
    mostrarTabuleiro tab
    putStrLn $ "Vez das " ++ show cor ++ ". Digite seu movimento ou 'sair' para encerrar:"
    entrada <- getLine
    if entrada == "sair"
        then putStrLn "Fim do jogo!"
        else do
            novoEstado <- processarEntrada entrada estado
            loopJogo novoEstado

processarEntrada :: String -> (Tabuleiro, Cor) -> IO (Tabuleiro, Cor)
processarEntrada entrada (tab, cor) = 
    case processarMovimento entrada tab cor of
        Just novoTabuleiro -> do
            let proximaCor = alternaJogador cor
            let emXeque = verificarXequeAposMovimento novoTabuleiro proximaCor
            if emXeque
                then putStrLn $ "Xeque! " ++ show proximaCor ++ " está em xeque."
                else return ()
            return (novoTabuleiro, proximaCor)
        Nothing -> do
            putStrLn "Movimento inválido!"
            return (tab, cor)

main :: IO ()
main = do
    putStrLn "Vamos iniciar o jogo de Xadrez!"
    loopJogo estadoInicial