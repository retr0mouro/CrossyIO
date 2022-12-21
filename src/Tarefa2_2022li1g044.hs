module Tarefa2_2022li1g044 where


import LI12223
import Data.List
--import System.Random

{-randomNumberGen :: RandomGen g => Int -> Int -> g -> Int
randomNumberGen x y j = fst (randomR (x,y) j) para F2-}


-- | = Função principal
-- | A função estendeMapa usa a auxiliar estendeTerreno para criar uma nova linha e retira a última linha da mesma lista com a ajuda da função init, criando um novo mapa
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa n t) i = (Mapa n ((estendeTerreno (Mapa n t) i):init t))


-- | A função auxiliar estendeTerreno cria uma nova linha com proximosTerrenosValidos e, usando a função estendeObstaculo, criar obstáculos

estendeTerreno :: Mapa -> Int -> (Terreno,[Obstaculo])
estendeTerreno (Mapa n t) i | i<=33 = ((head (proximosTerrenosValidos (Mapa n t))) , (estendeObstaculo ((head (proximosTerrenosValidos (Mapa n t))),[]) n i))
                            | i <=66 && i>33 = ((last (proximosTerrenosValidos (Mapa n t))) , (estendeObstaculo ((last (proximosTerrenosValidos (Mapa n t))),[]) n i)) 
                            | otherwise = (((!!) (proximosTerrenosValidos (Mapa n t)) 1) , (estendeObstaculo (((!!) (proximosTerrenosValidos (Mapa n t)) 1),[]) n i))

-- | A função estendeObstaculo usa o novo terreno gerado e a função proximosObstaculosValidos para criar obstáculos adequados 
estendeObstaculo :: (Terreno, [Obstaculo]) -> Int -> Int -> [Obstaculo]
estendeObstaculo (Relva, o) n i | i<=30 && length o <= n = (head (proximosObstaculosValidos n (Relva, o))): estendeObstaculo (Relva, o++[(head (proximosObstaculosValidos n (Relva, o)))]) (n-1) (mod (i-7) 100)
                                | i>30 && length o <= n = (last (proximosObstaculosValidos n (Relva, o))): estendeObstaculo (Relva, o++[(last (proximosObstaculosValidos n (Relva, o)))]) (n-1) (mod (i-7) 100)
                                | otherwise = take n o

estendeObstaculo (Rio _, o) n i | i<=30 && length o <= n = (head (proximosObstaculosValidos n (Rio 0, o))): estendeObstaculo (Rio 0, o++[(head (proximosObstaculosValidos n (Rio 0, o)))]) (n-1) (mod (i-7) 100)
                                | i>30 && length o <= n = (last (proximosObstaculosValidos n (Rio 0, o))): estendeObstaculo (Rio 0, o++[(last (proximosObstaculosValidos n (Rio 0, o)))]) (n-1) (mod (i-7) 100)
                                | otherwise = take n o

estendeObstaculo (Estrada _, o) n i | i<=30 && length o <= n = (head (proximosObstaculosValidos n (Estrada 0, o))): estendeObstaculo (Estrada 0, o++[(head (proximosObstaculosValidos n (Estrada 0, o)))]) (n-1) (mod (i-7) 100)
                                    | i>30 && length o <= n = (last (proximosObstaculosValidos n (Estrada 0, o))): estendeObstaculo (Estrada 0, o++[(last (proximosObstaculosValidos n (Estrada 0, o)))]) (n-1) (mod (i-7) 100)
                                    | otherwise = take n o


-- | A função proximosTerrenosValidos dá os possíveis terrenos a colocar na nova linha criada de acorda com os terrenos das linhas anteriores
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa n []) = [Relva, Relva]
proximosTerrenosValidos (Mapa n ((e ,o):t)) | isPrefixOf [Relva,Relva,Relva,Relva,Relva] (check ((e ,o):t)) = [Rio 2, Estrada 1, Estrada (-2)]
                                            | e == Relva = [Relva, Estrada 3, Rio 3]
                                            | isPrefixOf [Rio 0,Rio 0,Rio 0,Rio 0] (check ((e ,o):t)) = [Estrada 2, Estrada(-1), Relva] 
                                            | e == Rio 1 = [Rio (-2), Estrada (-2), Relva]
                                            | e == Rio 2 = [Rio (-1), Estrada (-1), Relva]
                                            | e == Rio (-1) = [Rio 2, Estrada 2, Relva]
                                            | e == Rio (-2) = [Rio 1, Estrada 1, Relva]
                                            | isPrefixOf [Estrada 0,Estrada 0,Estrada 0,Estrada 0,Estrada 0] (check ((e ,o):t)) = [Rio (-1), Rio 1, Relva]
                                            | e == Estrada 1 = [Estrada (-1), Estrada 2, Relva]
                                            | e == Estrada 2 = [Estrada 1, Estrada (-2), Relva]
                                            | e == Estrada (-1) = [Rio (-2), Estrada 2, Relva]
                                            | e == Estrada (-2) = [Rio (-1), Estrada 1, Relva]
                                            | otherwise = [Rio 1, Estrada 1, Relva]         
   

-- | Esta função é usada de maneira a colocar os terrenos todos com a mesma velocidade
check :: [(Terreno, [Obstaculo])] -> [Terreno]
check [] = []
check ((Relva ,o):t) = Relva:check t
check ((Rio _,o):t) = (Rio 0):check t
check ((Estrada _,o):t) = (Estrada 0):check t


-- | A função proximosObstaculosValidos dá os possíveis obstáculos a colocar na nova linha criada dependendo do tipo de terreno
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos n (Relva, o) = proximosrelva n (Relva, o)
proximosObstaculosValidos n (Rio _, o) = proximosrio n (Rio 0, o)
proximosObstaculosValidos n (Estrada _, o) = proximosestrada n (Estrada 0, o)


-- | A função proximosrelva dá possíveis obstáculos a colocar na nova linha criada de acordo com os obstáculos anteriores, quando o terreno é Relva
proximosrelva :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosrelva n (Relva, o) | elem Nenhum o == False = [Nenhum]
                           | otherwise = [Nenhum, Arvore]


-- | A função proximosrio dá possíveis obstáculos a colocar na nova linha criada de acordo com os obstáculos anteriores, quando o terreno é Rio
proximosrio :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosrio n (Rio _, o) | elem Tronco o == False = [Tronco]
                         | elem Nenhum o == False = [Nenhum]
                         | isInOf [Tronco,Tronco,Tronco,Tronco,Tronco] o == True = [Nenhum]
                         | otherwise = [Nenhum, Tronco]


-- | A função proximosestrada dá possíveis obstáculos a colocar na nova linha criada de acordo com os obstáculos anteriores, quando o terreno é Estrada
proximosestrada :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosestrada n (Estrada _, o) | elem Carro o == False = [Carro]
                                 | elem Nenhum o == False = [Nenhum]
                                 | isInOf [Carro,Carro,Carro] o == True = [Nenhum]
                                 | otherwise = [Nenhum, Carro]


-- | A função isInOf analisa se uma lista está dentro de outra lista ou não
isInOf :: Eq a => [a] -> [a] -> Bool
isInOf [] _ = True
isInOf _ [] = False
isInOf t o | t== head (group o) = True
           | otherwise = isInOf t (concat (tail (group o))){- |
Module      : Tarefa2_2022li1g044
Description : Geração contínua de um mapa
Copyright   : David Figueiredo  <a104360@alunos.uminho.pt>
              Diogo Ferreira <a104266@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
{-module Tarefa2_2022li1g044 where

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
-}


--auxEstMapa :: [Terreno] -> [Obstaculo] -> (Terreno,[Obstaculo]) 
--auxEstMapa x (l) (o) = (head (proximosTerrenosValidos x l), proximosObstaculosValidos x o) 
