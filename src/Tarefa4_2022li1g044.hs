{- |
Module      : Tarefa4_2022li1g044
Description : Determinar se o jogo terminou
Copyright   : David Figueiredo  <a104360@alunos.uminho.pt>
              Diogo Ferreira <a104266@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g044 where

import LI12223

-- | =Função jogoTerminou

-- | jogoTerminou : função que indica se o jogador morreu (True) ou está vivo (False)

jogoTerminou :: Jogo -> Bool 
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l ((t,z):xs))) = if x<0 || x>=l || y>=(length ((t,z):xs)) || morreu (Jogo (Jogador (x,y)) (Mapa l [saberTerreno y (((t,z):xs))])) then True else False

{- | 
=== Condições para o jogador perder

* A sua posição em x ser negativa

* A sua posição em x ser maior ou igual à largura do mapa

* A sua posição em y ser maior ou igual ao comprimento do mapa

* A sua posição em x ser igual à posição de um carro quando está numa estrada

* A sua posição em x ser igual à posição de um nenhum quando está num rio

-}

-- | ==Funções auxiliares 

-- | morreu : vê se o jogador se encontra na mesma posição que um carro ou se está numa posição de um rio sem tronco
morreu :: Jogo -> Bool 
morreu (Jogo (Jogador (x,y)) (Mapa n [])) = False
morreu (Jogo (Jogador (x,y)) (Mapa n ((Rio v,z):xs))) = if elem x (posicaoNenhumRio z) then True else False
morreu (Jogo (Jogador (x,y)) (Mapa n ((Estrada v,z):xs))) = if elem x (posicaoCarros z) then True else False
morreu (Jogo (Jogador (x,y)) (Mapa n ((Relva,z):xs))) = False

-- | posicaoCarros : gera um lista de inteiros com as posições dos carros na lista de obstáculos
posicaoCarros :: [Obstaculo] -> [Int] 
posicaoCarros (x:xs) = elemIndicess Carro (x:xs)
posicaoCarros [] = []

-- | posicaoNenhumRio : gera uma lista de inteiros com as posições  do nenhum numa lista de obstáculos
posicaoNenhumRio :: [Obstaculo] -> [Int] 
posicaoNenhumRio (x:xs) = elemIndicess Nenhum (x:xs)
posicaoNenhumRio [] = []

-- | elemIndicess : função que gera uma lista de inteiros com as posições em que o x se encontra na lista recebid
elemIndicess :: Eq a => a -> [a] -> [Int]
elemIndicess x [] = []
elemIndicess x (y:ys) | x == y = 0 : map (+1) (elemIndicess x ys)
                      | otherwise = map (+1) (elemIndicess x ys) 

-- | saberTerreno : função responsável por descobrir em que Terreno o jogador se encontra
saberTerreno :: Int -> [(Terreno,[Obstaculo])] -> (Terreno, [Obstaculo]) 
saberTerreno x ((t,z):xs) = if x == 0 then (t,z)
                            else saberTerreno (x-1) xs 
