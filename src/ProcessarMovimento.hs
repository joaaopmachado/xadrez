module ProcessarMovimento (
    processarMovimento,
    movimentoValido,
    caminhoLivre,
    caminhoDiagonal
) where

import Data.Char (isLower, isUpper)
import Tabuleiro (Tabuleiro, Posicao, Peca(..), Cor(..))
import Utils (colunaParaIndice, linhaParaIndice, pecaNaPosicao', charToPeca, corPeca)
import Data.Maybe (fromJust, isNothing, isJust)

-- Função para Verificar se o Movimento é Válido
movimentoValido :: Tabuleiro -> Peca -> Posicao -> Posicao -> Bool
movimentoValido tab peca inicio fim =
    let pecaDestino = pecaNaPosicao' fim tab
    in case peca of
        Rei _ -> abs (fst inicio - fst fim) <= 1 && abs (snd inicio - snd fim) <= 1 && not (mesmaCor peca pecaDestino)
        Peao cor -> movimentoPeaoValido cor inicio fim tab
        Cavalo _ -> ((abs (fst inicio - fst fim) == 1 && abs (snd inicio - snd fim) == 2) ||
                     (abs (fst inicio - fst fim) == 2 && abs (snd inicio - snd fim) == 1)) && not (mesmaCor peca pecaDestino)
        Torre _ -> (fst inicio == fst fim || snd inicio == snd fim) && caminhoLivre inicio fim tab && not (mesmaCor peca pecaDestino)
        Bispo _ -> abs (fst inicio - fst fim) == abs (snd inicio - snd fim) && caminhoLivre inicio fim tab && not (mesmaCor peca pecaDestino)
        Rainha _ -> (fst inicio == fst fim || snd inicio == snd fim || abs (fst inicio - fst fim) == abs (snd inicio - snd fim)) && caminhoLivre inicio fim tab && not (mesmaCor peca pecaDestino)
    where
        mesmaCor :: Peca -> Maybe Peca -> Bool
        mesmaCor _ Nothing = False
        mesmaCor p1 (Just p2) = corPeca p1 == corPeca p2

movimentoPeaoValido :: Cor -> Posicao -> Posicao -> Tabuleiro -> Bool
movimentoPeaoValido cor (x1, y1) (x2, y2) tab =
    let pecaDestino = pecaNaPosicao' (x2, y2) tab
    in case cor of
        Branca -> (x1 == x2 && y2 == y1 - 1 && isNothing pecaDestino) ||
                  (x1 == x2 && y1 == 6 && y2 == 4 && all (isNothing . flip pecaNaPosicao' tab) [(x1, 5), (x1, 4)]) ||
                  (abs (x1 - x2) == 1 && y2 == y1 - 1 && isJust pecaDestino && corPeca (fromJust pecaDestino) == Preta)
        Preta -> (x1 == x2 && y2 == y1 + 1 && isNothing pecaDestino) ||
                 (x1 == x2 && y1 == 1 && y2 == 3 && all (isNothing . flip pecaNaPosicao' tab) [(x1, 2), (x1, 3)]) ||
                 (abs (x1 - x2) == 1 && y2 == y1 + 1 && isJust pecaDestino && corPeca (fromJust pecaDestino) == Branca)

-- Função para Processar Movimento
processarMovimento :: String -> Tabuleiro -> Cor -> Maybe Tabuleiro
processarMovimento [c1, r1, c2, r2] tab corAtual =
    let inicio = (colunaParaIndice c1, linhaParaIndice r1)
        fim    = (colunaParaIndice c2, linhaParaIndice r2)
        peca   = pecaNaPosicao' inicio tab
    in case peca of
        Just p ->
            if corPeca p == corAtual && movimentoValido tab p inicio fim
            then Just (moverPeca inicio fim tab)
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

-- Função para Verificar se o Caminho Está Livre
caminhoLivre :: Posicao -> Posicao -> Tabuleiro -> Bool
caminhoLivre (x1, y1) (x2, y2) tab
  | x1 == x2 = all (\y -> isNothing (pecaNaPosicao' (x1, y) tab)) [min y1 y2 + 1 .. max y1 y2 - 1]
  | y1 == y2 = all (\x -> isNothing (pecaNaPosicao' (x, y1) tab)) [min x1 x2 + 1 .. max x1 x2 - 1]
  | otherwise = all (\(x, y) -> isNothing (pecaNaPosicao' (x, y) tab)) $ caminhoDiagonal (x1, y1) (x2, y2)

caminhoDiagonal :: Posicao -> Posicao -> [Posicao]
caminhoDiagonal (x1, y1) (x2, y2) =
  tail $ takeWhile (/= (x2, y2)) $ iterate next (x1, y1)
  where
    next (x, y) = (if x2 > x1 then succ x else pred x, if y2 > y1 then succ y else pred y)