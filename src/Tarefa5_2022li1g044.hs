module Tarefa5_2022li1g044 where 

import LI12223
import Tarefa1_2022li1g044
import Tarefa2_2022li1g044
import Tarefa3_2022li1g044
import Tarefa4_2022li1g044
import Graphics.Gloss
import System.Random

type World = (Jogo,[Int],Time)

type Time = Float 

ro = randNum (mkStdGen 1)
hi = head ro
--mapa = (Mapa 7 [(Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum]),(Estrada 2,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro]),(Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum]),(Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum]),(Estrada 2,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro]),(Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum])])

deslizaJogo :: Int -> Jogo -> Jogo 
deslizaJogo h (Jogo (Jogador (x,y)) (Mapa n l)) = if mapaValido (estendeMapa (Mapa n l) h) == True then (Jogo (Jogador (x,y)) (estendeMapa (Mapa n l) h)) else deslizaJogo (h+1) (Jogo (Jogador (x,y)) (Mapa n l))

--dvMapa :: Int -> Jogo -> 
--dvMapa i (Jogo j m) b = mapaValido m || 

--djFinal :: Int -> 

{-}
deslizaGame :: World -> World
deslizaGame ((Jogo (Jogador (x,y)) (Mapa n l)),p,(h:t),ti) = deslizaGame (deslizaJogo (((Jogo (Jogador (x,y-120)) (estendeMapa (Mapa n l) h)))),p,t,ti)-}

--firstLine :: Mapa -> Mapa 
--firstLine (Mapa n (h:t)) = 

--randNuma :: [Int] -> Int
--randNuma (h:t) 

randNum :: StdGen -> [Int]
randNum gen =
            let (firstNum, newGen) = random gen
            in  ((mod firstNum 100) : (randNum newGen))



