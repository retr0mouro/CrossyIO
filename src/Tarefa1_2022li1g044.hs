{- |
Module      : Tarefa1_2022li1g044
Description : Validação de um mapa
Copyright   : David Figueiredo  <a104360@alunos.uminho.pt>
              Diogo Ferreira <a104266@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g044 where

import LI12223
import Data.List
import System.Random

{- | =Função mapaValido

===Função mapaValido verifica se:

*Não existem obstáculos em terrenos __impróprios__ .

*Rios contíguos têm direções __opostas__ .

*Troncos têm, no máximo, __5__ unidades de comprimento.

*Carros têm, no máximo, __3__ unidades de comprimento.

*Em qualquer linha tem de existir um __Nenhum__ na lista de obstáculos, a lista __não pode__ ser composta apenas por /obstáculos/ . 

*O comprimento da lista de /obtáculos/ corresponde à larguro do mapa.

*Contiguamente, não devem existir mais do que __4 rios__ , nem __5 estradas__ ou __relvas__ .

-}


mapaValido :: Mapa -> Bool
mapaValido (Mapa a []) = True
mapaValido (Mapa _ [(Estrada _, _ ),(Estrada _, _ ),(Estrada _, _ ),(Estrada _, _ ),(Estrada _, _),(Estrada _, _)]) = False
mapaValido (Mapa _ [(Relva, _ ),(Relva, _ ),(Relva, _ ),(Relva, _ ),(Relva, _),(Relva, _)]) = False
mapaValido (Mapa n ((Relva ,o):(Relva ,os):ms)) 
    | mapaNenhum (Mapa n ((Relva ,o):(Relva ,os):ms)) == [True] = True 
    | otherwise = False 
mapaValido (Mapa _ [(Rio _, _ ),(Rio _, _ ),(Rio _, _ ),(Rio _, _ ),(Rio _, _)]) = False
mapaValido (Mapa l ((t, o):xs)) 
    | length o > l || length o < l = False 
    | isRelva t && (elem Carro o || elem Tronco o) = False      
    | isRio t && (elem Arvore o || elem Carro o) = False        
    | isEstrada t && (elem Tronco o || elem Arvore o) = False   
    | notElem Nenhum o = False                                           
    | isPrefixOf (car) (compCarros o) = False                          
    | isPrefixOf (tron) (compTroncos o) = False                        
    | null xs = True                                                           
    | isRio t && isRio (fst hxs) && not (riosCont t (fst hxs)) = False 
    | not $ mapaValido (Mapa l xs) = False                                 
    | otherwise = True                                                          
    where hxs = head xs 
          car = [Carro,Carro,Carro,Carro]
          tron = [Tronco,Tronco,Tronco,Tronco,Tronco]  

 -- | ==Auxiliares do mapaValido

-- | riosCont : verifica se as velocidades de rios sao uma positiva e uma negativa
riosCont :: Terreno -> Terreno -> Bool
riosCont (Rio a) (Rio b) = not ((a<0 && b<0) || (a>0 && b>0))

-- | isRelva : verifica se o terreno é relva
isRelva :: Terreno -> Bool
isRelva a = a == Relva

-- | isRio : verifica se o terreno é Rio
isRio :: Terreno -> Bool
isRio (Rio v) = True 
isRio (Estrada a) = False
isRio Relva = False

-- | isEstrada : verifica se o terreno é Estrada
isEstrada :: Terreno -> Bool
isEstrada (Estrada a) = True
isEstrada (Rio a) = False
isEstrada Relva = False

-- | isTronco : verifica se o obstáculo é Tronco
isTronco :: Obstaculo -> Bool
isTronco a = a == Tronco

-- | isCarro : verifica se o osbstáculo é um Carro
isCarro :: Obstaculo -> Bool
isCarro a = a == Carro


-- | compCarros : se o comprimento da lista for maior do que 4 então aplica-se a função take aos primeiros 4 elementos da lista, ou, se for menor, a função dá a lista de volta
compCarros :: [Obstaculo] -> [Obstaculo]
compCarros [] = []
compCarros x
    | isPrefixOf [Carro,Carro,Carro,Carro] (take 4 x) = [Arvore]     --isPrefixOf [Tronco,Tronco,Tronco,Tronco] (drop 1 x)
    | otherwise = compCarros (drop 1 x) 

-- | compTroncos : se o comprimento da lista for maior do que 6 então aplica-se a função take aos primeiros 6 elementos da lista, ou, se for menor, a função dá a lista de volta
compTroncos :: [Obstaculo] -> [Obstaculo]
compTroncos [] = []
compTroncos x
    | isPrefixOf [Tronco,Tronco,Tronco,Tronco,Tronco,Tronco] (take 6 x) = [Arvore]     --isPrefixOf [Tronco,Tronco,Tronco,Tronco] (drop 1 x)
    | otherwise = compTroncos (drop 1 x)

-- | mapaNenhum : verifica se existe um caminho possivel se houver duas relvas seguidas
mapaNenhum :: Mapa -> [Bool]
mapaNenhum (Mapa l ((Relva, o):(Relva,os):t)) 
    | elemW (elemIndices Nenhum o) (elemIndices Nenhum os) || elemW (elemIndices Nenhum os) (elemIndices Nenhum o) = [True]
    | otherwise = [False]

-- | elemW : redefinição da função elem 
elemW :: Eq a => [a] -> [a] -> Bool
elemW _ [] = False 
elemW [] _ = False 
elemW (h:t) l
    | elem h l = True
    | otherwise = elemW t l  
 