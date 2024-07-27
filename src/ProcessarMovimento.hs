module ProcessarMovimento (
    processarMovimento
) where

import Tabuleiro (Tabuleiro, Posicao, Peca(..), Cor(..))
import Utils (colunaParaIndice, linhaParaIndice, pecaNaPosicao', charToPeca, corPeca)
import ValidacaoMovimento (movimentoValido)
import Check (verificarXeque)

import Data.Maybe (fromJust, isNothing, isJust)

-- Função para Processar Movimento
processarMovimento :: String -> Tabuleiro -> Cor -> Maybe Tabuleiro
processarMovimento [c1, r1, c2, r2] tab corAtual =
    let inicio = (colunaParaIndice c1, linhaParaIndice r1)
        fim    = (colunaParaIndice c2, linhaParaIndice r2)
        peca   = pecaNaPosicao' inicio tab
    in case peca of
        Just p ->
            if corPeca p == corAtual && movimentoValido tab p inicio fim
            then
                let novoTabuleiro = moverPeca inicio fim tab
                in if not (verificarXeque novoTabuleiro corAtual)
                   then Just novoTabuleiro
                   else Nothing
            else Nothing
        Nothing -> Nothing
processarMovimento _ _ _ = Nothing

moverPeca :: Posicao -> Posicao -> Tabuleiro -> Tabuleiro
moverPeca (x1, y1) (x2, y2) tab =
    let
        -- Obtém as linhas do tabuleiro
        linhaInicial = tab !! y1
        linhaFinal = tab !! y2
        
        -- Obtém a peça a ser movida
        peca = linhaInicial !! x1
        
        -- Atualiza a linha inicial removendo a peça da posição (x1, y1)
        linhaInicialAtualizada = take x1 linhaInicial ++ " " ++ drop (x1 + 1) linhaInicial
        
        -- Atualiza a linha final adicionando a peça na posição (x2, y2)
        linhaFinalAtualizada = take x2 linhaFinal ++ [peca] ++ drop (x2 + 1) linhaFinal
        
        -- Atualiza o tabuleiro com as novas linhas
        tabIntermediario = take y1 tab ++ [linhaInicialAtualizada] ++ drop (y1 + 1) tab
        tabFinal = take y2 tabIntermediario ++ [linhaFinalAtualizada] ++ drop (y2 + 1) tabIntermediario
    in
        tabFinal

