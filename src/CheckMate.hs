module CheckMate (verificarXequeMate) where

import Tabuleiro
import Check (verificarXeque)
import ValidacaoMovimento (movimentoValido)
import Utils (pecaNaPosicao', charToPeca, corPeca)
import Data.Maybe (isJust, fromJust)

-- Função para verificar se o jogador está em xeque-mate
verificarXequeMate :: Tabuleiro -> Cor -> Bool
verificarXequeMate tab cor = 
    let pecas = todasPecasDoJogador tab cor
        movimentosPossiveis = concatMap (\(pos, peca) -> movimentosValidosParaPeca pos peca tab) pecas
    in all (\novoTab -> verificarXeque novoTab cor) (map (executarMovimento tab) movimentosPossiveis)

-- Função para obter todas as peças de um jogador
todasPecasDoJogador :: Tabuleiro -> Cor -> [(Posicao, Peca)]
todasPecasDoJogador tab cor = 
    filter (\(_, peca) -> corPeca peca == cor) (todasPecas tab)

-- Função para obter todos os movimentos válidos para uma peça
movimentosValidosParaPeca :: Posicao -> Peca -> Tabuleiro -> [(Posicao, Posicao)]
movimentosValidosParaPeca inicio peca tab = 
    [(inicio, fim) | x <- [0..7], y <- [0..7], let fim = (x, y), movimentoValido tab peca inicio fim]

-- Função para executar um movimento no tabuleiro e retornar o novo tabuleiro
executarMovimento :: Tabuleiro -> (Posicao, Posicao) -> Tabuleiro
executarMovimento tab (inicio, fim) = 
    moverPeca inicio fim tab

-- Função para mover uma peça de uma posição para outra (definida em ProcessarMovimento)
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

-- Função para obter todas as peças no tabuleiro
todasPecas :: Tabuleiro -> [(Posicao, Peca)]
todasPecas tab = 
    [((x, y), charToPeca (tab !! y !! x)) | x <- [0..7], y <- [0..7], tab !! y !! x /= ' ']