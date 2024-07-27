module ValidacaoMovimento (
    movimentoValido,
    caminhoLivre,
    caminhoDiagonal
) where

import Data.Char (isLower, isUpper)
import Tabuleiro (Tabuleiro, Posicao, Peca(..), Cor(..))
import Utils (pecaNaPosicao', corPeca)
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
