module Tarefa5_2022li1g044 where 

import LI12223
import Tarefa1_2022li1g044
import Tarefa2_2022li1g044
import Tarefa3_2022li1g044
import Tarefa4_2022li1g044
import Graphics.Gloss
import System.Random



-- | deslizaJogo : Função utilizada para estender o mapa a cada atualização, com algum grau de aleatoriedade, recorrendo à função estendeMapa
deslizaJogo :: Int -> Jogo -> Jogo 
deslizaJogo h (Jogo (Jogador (x,y)) (Mapa n l)) =  Jogo (Jogador (x,y)) (estendeMapa (Mapa n l) h)




