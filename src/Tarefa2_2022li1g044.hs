{- |
Module      : Tarefa2_2022li1g044
Description : Geração contínua de um mapa
Copyright   : David Figueiredo  <a104360@alunos.uminho.pt>
              Diogo Ferreira <a104266@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g044 where

import LI12223
import Data.List
import Data.Char 
import System.Random


{-| = Função estendeMapa

    ===Função estendeMapa elimina a última linha do mapa, adicionando uma nova linha.
-}

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa n t) i = (Mapa n ((estTerreno (Mapa n t)  (i)):init t))
{-
estendeMapa (Mapa l []) _ = (Mapa l [])
estendeMapa (Mapa l [x]) _ = (Mapa l [x])
estendeMapa (Mapa l ((t,[o]):xs)) n
    | n >= 0 && n <= 33 = (Mapa l ((estTerreno (Mapa l ((t,[o]):xs)) n):(init ((t,[o]):xs)))) --(Mapa l [(head (proximosTerrenosValidos (Mapa l [(t,[o])])), proximosObstaculosValidos l (t,[o]))])
   -- | n >= 34 && n <= 66 = (Mapa l [(head ( tail (proximosTerrenosValidos (Mapa l [(t,[o])]))), proximosObstaculosValidos l (t,[o]))])
    | otherwise = (Mapa l [(head ( reverse (proximosTerrenosValidos (Mapa l [(t,[o])]))), proximosObstaculosValidos l (t,[o]))])
-
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)
-}


-- | = Função proximosTerrenosValidos 
-- === Revela a lista dos proximos terrenos passíveis de escolha para uma próxima linha do mapa dado
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Relva, Relva]
--proximosTerrenosValidos (Mapa l [(Rio 0, _ ),(Rio 0, _ ),(Rio 0, _ ),(Rio 0, _ )]) = [Estrada 0, Relva]
--proximosTerrenosValidos (Mapa l [(Estrada 0 , _),(Estrada 0, _),(Estrada 0, _),(Estrada 0, _),(Estrada 0, _)]) = [Rio 0, Relva]
--proximosTerrenosValidos (Mapa l [(Relva, _),(Relva, _),(Relva, _),(Relva, _),(Relva, _)]) = [Estrada 0, Rio 0]
proximosTerrenosValidos (Mapa l ((t , o):r)) 
    | isPrefixOf [Relva,Relva,Relva,Relva,Relva] (verifica ((t ,o):r)) = [Rio 2, Estrada 1, Estrada (-2)]
    | t == Relva = [Relva, Estrada 2, Rio (-2)]
    | t == Estrada 1 = [Estrada (-1), Estrada 2, Relva]
    | t == Estrada 2 = [Estrada 1, Estrada (-2), Relva]
    | t == Estrada (-1) = [Rio (-2), Estrada 2, Relva]
    | t == Estrada (-2) = [Rio (-1), Estrada 3, Rio 3]
    | isPrefixOf [Rio 69,Rio 69,Rio 69,Rio 69] (verifica ((t ,o):r)) = [Estrada 3, Estrada(-1), Relva]
    | t == Rio 1 = [Rio (-1), Estrada (-2), Relva]
    | t == Rio 2 = [Rio (-2), Estrada (-1), Relva]
    | t == Rio (-1) = [Rio 2, Estrada 1, Relva]
    | t == Rio (-2) = [Rio 1, Estrada 1, Relva]
    | otherwise = [Rio 1, Estrada 1, Relva, Rio 3, Estrada 3]

{-| = Função estTerreno 
    === A função determina uma lista com alguma pseudo-aleatoriedade de um terreno com uma lista de obstáculos, tal que formem a proxima linha do mapa. 
        Por outras palavras, "estende" efetivamente o mapa, por dar a próxima linha possível do mapa-}
estTerreno :: Mapa -> Int -> (Terreno, [Obstaculo])
estTerreno (Mapa n t) i 
    | (mod i 100) <= 33 = ((head (drop (a) (proximosTerrenosValidos (Mapa n t)))) , (estObstaculo ((head (drop (a) (proximosTerrenosValidos (Mapa n t)))),[]) n i))
    | (mod i 100) <= 66 && (mod i 100) > 33 = ((head (drop a (proximosTerrenosValidos (Mapa n t)))) , (estObstaculo (head (drop a(proximosTerrenosValidos (Mapa n t))),[]) n i))
    | i == 0 = estTerreno (Mapa n t) (i-7)  
    | otherwise = (((!!) (proximosTerrenosValidos (Mapa n t)) 1) , (estObstaculo (((!!) (proximosTerrenosValidos (Mapa n t)) 1),[]) n i))
    where a = (mod i 2)

{-| = Função estObstaculo
    === A função é utilizada para, dada uma linha do mapa e se esta não estiver completa, gerar uma linha completa, com alguma pseudo-aleatoriedade-}
estObstaculo :: (Terreno,[Obstaculo]) -> Int -> Int -> [Obstaculo] 
estObstaculo (Rio a , o) n i
    | i == 0 = estObstaculo (Rio a, o) n (i + 3)
    | length o /= n && i <= 50 = estObstaculo (Rio a,(o ++ [Tronco])) n (i*2)
    | length o /= n && i >= 51 = estObstaculo (Rio a,(o ++ [Nenhum])) n (mod i 23)
    | otherwise = o

estObstaculo (Estrada a , o) n i
    | i == 0 = estObstaculo (Estrada a, o) n (i+9)
    | length o /= n && i <= 50 = estObstaculo (Estrada 0,(o ++ [Carro])) n (i*2)
    | length o /= n && i >= 51 = estObstaculo (Estrada 0,(o ++ [Nenhum])) n (mod i 57)
    | otherwise = o

estObstaculo (Relva, o) n i | i == 0 = estObstaculo (Relva, o) n (i + 69)
                            | length o /= n && i <= 50 = estObstaculo (Relva,(o ++ [Arvore])) n (mod ((i+1)*2) 100) 
                            | length o /= n && i >= 51 = estObstaculo (Relva,(o ++ [Nenhum])) n (mod (i + 55) 71)
                            | otherwise = o 

{-| verifica: listar os terrenos presentes no mapa-}
verifica :: [(Terreno, [Obstaculo])] -> [Terreno]
verifica [] = []
verifica ((Relva ,_):t) = Relva:verifica t
verifica ((Rio v,_):t) = (Rio v):verifica t
verifica ((Estrada v,_):t) = (Estrada v):verifica t

{-| Analogamente, a funçao __próximosObstáculosVálidos__ deve gerar a lista
de obstáculos passíveis de serem usados para continuar uma dada linha do
mapa-}
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos x (Estrada _, []) = [Nenhum ,Carro]
proximosObstaculosValidos x (Estrada _, y) 
    | length y == x = [] 
    | (Nenhum `elem` y) == False = [Nenhum]
    | (length y == x-1) == True && (Carro `elem` y) == False = [Carro]  
    | otherwise = [Nenhum ,Carro]
proximosObstaculosValidos x (Rio _, []) = [Nenhum ,Tronco]
proximosObstaculosValidos x (Rio _, y) 
    | length y == x = [] 
    | (Tronco `elem` y) == False = [Tronco] 
    | (Nenhum `elem` y) == False = [Nenhum] 
    | (length y == x-1) == True && (Tronco `elem` y) == False = [Tronco]
    | otherwise = [Nenhum ,Tronco]
proximosObstaculosValidos x (Relva , []) = [Nenhum ,Arvore] 
proximosObstaculosValidos x (Relva , y) 
    | length y == x = [] 
    | (Nenhum `elem` y) == False = [Nenhum]
    | otherwise = [Nenhum,Arvore]



--auxEstMapa :: [Terreno] -> [Obstaculo] -> (Terreno,[Obstaculo]) 
--auxEstMapa x (l) (o) = (head (proximosTerrenosValidos x l), proximosObstaculosValidos x o) 
