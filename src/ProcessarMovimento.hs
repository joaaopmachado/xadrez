module ProcessarMovimento (
    processarMovimento,
) where

import Check (verificarXeque)
import Tabuleiro (Cor (..), Peca (..), Posicao, Tabuleiro)
import Utils (charToPeca, colunaParaIndice, corPeca, linhaParaIndice, pecaNaPosicao')
import ValidacaoMovimento (movimentoValido, roqueValido)

import Data.Maybe (fromJust, isJust, isNothing)

-- Função para Processar Movimento
processarMovimento :: String -> Tabuleiro -> Cor -> Maybe Tabuleiro
processarMovimento [c1, r1, c2, r2] tab corAtual =
    let inicio = (colunaParaIndice c1, linhaParaIndice r1)
        fim = (colunaParaIndice c2, linhaParaIndice r2)
        peca = pecaNaPosicao' inicio tab
     in case peca of
            Just p ->
                if corPeca p == corAtual && movimentoValido tab p inicio fim
                    then
                        let novoTabuleiro = if roqueValido tab p inicio fim then executarRoque inicio fim tab else moverPeca inicio fim tab
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

-- Função para executar o roque
executarRoque :: Posicao -> Posicao -> Tabuleiro -> Tabuleiro
executarRoque (x1, y1) (x2, y2) tab
    | x2 == 6 = moverPeca (7, y1) (5, y1) (moverPeca (x1, y1) (x2, y2) tab) -- Roque pequeno
    | x2 == 2 = moverPeca (0, y1) (3, y1) (moverPeca (x1, y1) (x2, y2) tab) -- Roque grande
    | otherwise = tab
