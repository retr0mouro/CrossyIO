{-
Module      : Tarefa3_2022li1g044
Description : Movimentação do personagem e obstáculos
Copyright   : David Figueiredo  <a104360@alunos.uminho.pt>
              Diogo Ferreira <a104266@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g044 where

import LI12223

-- | =Funcão Principal animaJogo

-- | animaJogo : recebe um jogo e realiza as jogadas
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs))) w  = juntarJogos (Jogo (Jogador (x,y)) (Mapa n (parte1 ((t,z):xs) (x,y)))) (moveJogo (Jogo (Jogador (x,y)) (Mapa n (parte2 ((t,z):xs) (x,y)))) (w))

-- | ==Auxiliares do anima Jogo

-- | moveJogo : realiza as jogadas
moveJogo :: Jogo -> Jogada -> Jogo 
moveJogo (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs))) Parado = parado (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs))) 
moveJogo (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs))) (Move Cima) = moverCima (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))
moveJogo (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs))) (Move Baixo) = moverBaixo (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))
moveJogo (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs))) (Move Direita) = moverDireita (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))
moveJogo (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs))) (Move Esquerda) = moverEsquerda (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))

-- | juntarJogos: recebe dois jogos e junta num só jogo
juntarJogos :: Jogo -> Jogo -> Jogo  
juntarJogos (Jogo (Jogador (x,y)) (Mapa n to)) (Jogo (Jogador (x1,y2)) (Mapa n2 to2)) = Jogo (Jogador (x1,y2)) (Mapa n2 (to ++ to2))

-- | moverObsC : move os obstáculos que estão em cima do jogador 
moverObsC :: [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])] 
moverObsC [] = []
moverObsC ((t,z):xs) = moverObs ((t,z):xs)

-- | parte2 : divide o mapa e dá como resultado a linha onde o jogador se encontra, todo o que está em baixo e uma linha em cima
parte2 :: [(Terreno, [Obstaculo])] -> Coordenadas -> [(Terreno, [Obstaculo])] 
parte2 ((t,z):xs) (x,y)   | y <= 1 = let  (as,a : bs) = splitAt (y-2) ((t,z):xs)
                                        in (recuperaLinha ((t,z):xs) :bs) 
                          | otherwise = let  (as,a : bs) = splitAt (y-2) ((t,z):xs)
                                           in bs

-- | recuperaLinha : tem como resultado a linha que está em cima do jogador 
recuperaLinha :: [(Terreno, [Obstaculo])] -> (Terreno, [Obstaculo]) 
recuperaLinha ((x, z):y) = (x,z)

-- | parte1 : divide o mapa e dá como resultado todo o que está em cima da linha em cima da que o jogador se encontra
parte1 :: [(Terreno, [Obstaculo])] -> Coordenadas -> [(Terreno, [Obstaculo])] 
parte1 ((t,z):xs) (x,y) = let (as,a : bs) = splitAt (y-1) ((t,z):xs)
                          in moverObsC as
 
-- | ==Mover Obstaculos

{- | === moverObs : mover os obstaculos dependendo da velocidade do terreno

*quando a velocidadde for maior que 0, vai se mover para a direita x unidades, sendo x o valor da velocidade 

*quando a velocidade for menor que 0, vai se mover para a esquerda x unidades, sendo x o valor da velocidade

*quando a velocidade for igual a 0, os obstaculos nao se vao deslocar

*como a relva nao tem velocidade, os obstaculos nao se vao mover
-}
moverObs :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]   
moverObs [] = []
moverObs ((Rio x, y:ys):b) = if x > 0 then ((Rio x, (desldir x (y:ys)))):moverObs b 
                                 else if x < 0 then ((Rio x, (deslesq2 x (y:ys)))):moverObs b 
                                 else ((Rio x, y:ys)):moverObs b 
moverObs ((Estrada x, y:ys):b) = if x > 0 then ((Estrada x, (desldir x (y:ys)))):moverObs b 
                                 else if x < 0 then ((Estrada x, (deslesq2 x (y:ys)))):moverObs b 
                                 else ((Estrada x, y:ys)):moverObs b 
moverObs ((Relva, z):b) = (Relva, z):moverObs b 

-- | ==Auxiliares do Mover Obstaculos

-- | deslesq2 : função auxiliar para mover os obstáculos para a esquerda
deslesq2 :: Int -> [a] -> [a]
deslesq2 n [] = []
deslesq2 n m = let x = mod (abs n)  (length m) in
                  drop x m ++ take x m 

-- | desldir : função auxiliar para mover os obstáculos para a direita
desldir :: Int -> [a] -> [a] 
desldir n [] = []
desldir n l = trocarOrdem (take n (trocarOrdem l)) ++ trocarOrdem (drop n (trocarOrdem l))

-- | trocarOrdem: função auxiliar para o desldir
trocarOrdem :: [a] -> [a] 
trocarOrdem [] = []
trocarOrdem (x:xs) = trocarOrdem xs ++ [x]

-- | =Mover Jogador 

-- | ==Jogador Parado

-- | parado : função para quando a ação do jogador é parado
parado :: Jogo -> Jogo
parado (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs))) = if y == 0 then parado2 (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))
                                                     else parado3 (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))

-- | ===Auxiliares para Parado

-- | parado2 : parado quando o y é 0
parado2 :: Jogo -> Jogo
parado2 (Jogo (Jogador (x,y)) (Mapa n ((Estrada v,z):xs))) = Jogo (Jogador (x,y)) (Mapa n (moverObsE x ((Estrada v,z):xs)))
parado2 (Jogo (Jogador (x,y)) (Mapa n ((Relva,z):xs))) = Jogo (Jogador (x,y)) (Mapa n  (moverObs ((Relva,z):xs)))
parado2 (Jogo (Jogador (x,y)) (Mapa n ((Rio v,z):xs))) = if x `elem` posicaoTroncos z then Jogo (posicaoJ n v (Jogador (x,y))) (Mapa n (moverObs ((Rio v,z):xs)))
                                                          else Jogo (Jogador (n+1,y)) (Mapa n (moverObs ((Rio v,z):xs)))

-- | parada3 : parado quando o y é diferente de 0
parado3 :: Jogo -> Jogo
parado3 (Jogo (Jogador (x,y)) (Mapa n ((t,lo):(tr,z):[]))) = Jogo (Jogador ((-1),y-1)) (Mapa n  (moverObs ((t,lo):(tr,z):[]))) -- quando fica parado e sai da area do mapa ele vai para posicao negativa
parado3 (Jogo (Jogador (x,y)) (Mapa n ((t,lo):(Relva,z):xs))) = Jogo (Jogador (x,y)) (Mapa n  (moverObs ((t,lo):(Relva,z):xs)))
parado3 (Jogo (Jogador (x,y)) (Mapa n ((t,lo):(Rio v,z):xs))) = if x `elem` posicaoTroncos z then Jogo (posicaoJ n v (Jogador (x,y))) (Mapa n (moverObs ((t,lo):(Rio v,z):xs)))
                                                                 else Jogo (Jogador (n+1,y)) (Mapa n (moverObs ((t,lo):(Rio v,z):xs)))
parado3 (Jogo (Jogador (x,y)) (Mapa n ((t,lo):(Estrada v,z):xs))) = Jogo (Jogador (x,y)) (Mapa n (moverObsE x ((t,lo):(Estrada v,z):xs)))

-- | posicaoJ : faz com que o jogador acompanhe o tronco e mete o jogador fora do mapa caso o tronco saia do mapa e troque de lado 
posicaoJ :: Int -> Int -> Jogador -> Jogador
posicaoJ n v (Jogador (x,y)) = if  x+v <= n && x+v >= 0 then Jogador (x+v,y)
                              else Jogador (n+1,y)

-- | posicaoTroncos : dá uma lista de inteiros com as posições dos troncos numa lista de obstáculos
posicaoTroncos :: [Obstaculo] -> [Int]
posicaoTroncos [] = []
posicaoTroncos (x:xs) = elemIndicess Tronco (x:xs)

-- | ==Jogador a Mover para Cima

-- | moverCima : função para quando a ação do jogador é Move Cima
moverCima :: Jogo -> Jogo
moverCima (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs))) | y == 0 = parado (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))
                                                 | otherwise = moverCima1 (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))

-- | ===Auxiliares para mocerCima

-- | moverCima1 : mover para cima quando y é diferente de 0 
moverCima1 :: Jogo -> Jogo
moverCima1 (Jogo (Jogador (x,y)) (Mapa n ((Relva,z):(t,zs):xs))) = if x `elem` posicaoArvores z then Jogo (Jogador (x,y)) (Mapa n (moverObs ((Relva,z):(t,zs):xs)))
                                                                   else Jogo (Jogador coordenadas1) (Mapa n (moverObs ((Relva,z):(t,zs):xs)))
  where coordenadas1 = (x,y-1)
moverCima1 (Jogo (Jogador (x,y)) (Mapa n ((ts,lo):(Relva,z):xs))) = Jogo (Jogador coordenadas1) (Mapa n (moverObs ((ts,lo):(Relva,z):xs)))
  where coordenadas1 = (x,y-1)
moverCima1 (Jogo (Jogador (x,y)) (Mapa n ((ts,lo):(Rio v,z):xs))) = Jogo (Jogador coordenadas1) (Mapa n (moverObs ((ts,lo):(Rio v,z):xs)))
  where coordenadas1 = (x,y-1)
moverCima1 (Jogo (Jogador (x,y)) (Mapa n ((ts,lo):(Estrada v,z):xs))) = Jogo (Jogador coordenadas1) (Mapa n (moverObsE x ((ts,lo):(Estrada v,z):xs)))
  where coordenadas1 = (x,y-1)

-- | ==Jogador a Mover para Baixo

-- | moverBaixo : função para quando a ação do jogador é Move Baixo
moverBaixo :: Jogo -> Jogo
moverBaixo (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs))) = if y == 0 then moverBaixo2 (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))
                                                    else moverBaixo3 (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))

-- | ===Auxiliares para moverBaixo

-- | moverBaixo2 : move para baixo quando y é 0
moverBaixo2 :: Jogo -> Jogo
moverBaixo2 (Jogo (Jogador (x,y)) (Mapa n ((t,z):(Relva,zs):xs))) = if x `elem` posicaoArvores zs then  parado (Jogo (Jogador (x,y)) (Mapa n ((t,z):(Relva,zs):xs)))
                                                                      else let coordenadas2 = (x,y+1) in Jogo (Jogador coordenadas2) (Mapa n (moverObs ((t,z):(Relva,zs):xs)))
moverBaixo2 (Jogo (Jogador (x,y)) (Mapa n ((Estrada v,z):xs))) = let coordenadas2 = (x,y+1) in Jogo (Jogador coordenadas2) (Mapa n (moverObsE x ((Estrada v,z):xs)))
moverBaixo2 (Jogo (Jogador (x,y)) (Mapa n ((Rio v,z):xs))) = let coordenadas2 = (x,y+1) in Jogo (Jogador coordenadas2) (Mapa n (moverObs ((Rio v,z):xs)))
moverBaixo2 (Jogo (Jogador (x,y)) (Mapa n ((Relva,z):xs))) = let coordenadas2 = (x,y+1) in Jogo (Jogador coordenadas2) (Mapa n (moverObs ((Relva,z):xs)))

-- | moverBaixo3 : move para baixo quando y é diferente de 0
moverBaixo3 :: Jogo -> Jogo
moverBaixo3 (Jogo (Jogador (x,y)) (Mapa n ((ts,lo):(t,z):(Relva,zs):xs))) = if x `elem` posicaoArvores zs then parado (Jogo (Jogador (x,y)) (Mapa n ((ts,lo):(t,z):(Relva,zs):xs)))
                                                                              else let coordenadas2 = (x,y+1) in Jogo (Jogador coordenadas2 ) (Mapa n (moverObs ((ts,lo):(t,z):(Relva,zs):xs)))
moverBaixo3 (Jogo (Jogador (x,y)) (Mapa n ((ts,lo):(Rio v,zs):xs))) = let coordenadas2 = (x,y+1) in Jogo (Jogador coordenadas2 ) (Mapa n (moverObs ((ts,lo):(Rio v,zs):xs)))
moverBaixo3 (Jogo (Jogador (x,y)) (Mapa n ((ts,lo):(Estrada v,zs):xs))) = let coordenadas2 = (x,y+1) in Jogo (Jogador coordenadas2 ) (Mapa n (moverObsE x ((ts,lo):(Estrada v,zs):xs)))
moverBaixo3 (Jogo (Jogador (x,y)) (Mapa n ((ts,lo):(Relva ,zs):xs))) = let coordenadas2 = (x,y+1) in Jogo (Jogador coordenadas2 ) (Mapa n (moverObs ((ts,lo):(Relva ,zs):xs)))

--acrescentei o (t,z) caso n funcione 

-- | ==Mover para a Direita

-- | moverDireita : função para quando a ação do jogador é Move Direita
moverDireita :: Jogo -> Jogo
moverDireita (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs))) = if x>=(n-1) then parado (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))
                                                           else moverDireitaax (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))

-- | ===Auxiliares para moverDireita

-- | moverDireitaax : divide a função direita no caso em que y == o e y é diferente de 0
moverDireitaax :: Jogo -> Jogo
moverDireitaax (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs))) = if y == 0 then moverDireita2 (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))
                                                             else moverDireita3 (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))

-- | moverDireita2 : faz o caso em que y é igual a 0 da funcção moverDireitaax
moverDireita2 :: Jogo -> Jogo
moverDireita2 (Jogo (Jogador (x,y)) (Mapa n ((Rio v,z):xs)))  | v<0 && elem x (posicaoTroncos z) && elem (x+v+1) (posicaoTroncos (moverObs2 (Rio v,z))) = Jogo (Jogador (x+v+1,y)) (Mapa n (moverObs ((Rio v,z):xs)))
                                                              | v>0 && elem x (posicaoTroncos z) && elem (x+v+1) (posicaoTroncos (moverObs2 (Rio v,z)))  = Jogo (Jogador (x+v+1,y)) (Mapa n (moverObs ((Rio v,z):xs)))
                                                              | otherwise = Jogo (Jogador ((-1),y)) (Mapa n (moverObs ((Rio v,z):xs))) --o boneco esta morto se as condicoes de cima nao acontecerem logo colocamos o boneco fora do mapa para ser mais facil de dizer que ele está morto
moverDireita2 (Jogo (Jogador (x,y)) (Mapa n ((Estrada v,z):xs)))  | v/=0 && (elem x (posicaoCarros z) == False) && (elem (x+1) ((posicaoCarros (moverObsE2 x ((Estrada v,z))))) == False) = Jogo (Jogador (x+1,y)) (Mapa n (moverObsE (x+1) ((Estrada v,z):xs)))
                                                                  | otherwise = Jogo (Jogador ((-1),y)) (Mapa n (moverObsE (x+1) ((Estrada v,z):xs))) --o boneco esta morto se as condicoes de cima nao acontecerem logo colocamos o boneco fora do mapa para ser mais facil de dizer que ele está morto
moverDireita2 (Jogo (Jogador (x,y)) (Mapa n ((Relva,z):xs)))  | elem (x+1) (posicaoArvores z) = parado (Jogo (Jogador (x,y)) (Mapa n ((Relva,z):xs)))
                                                              | otherwise = Jogo (Jogador (x+1,y)) (Mapa n (moverObs ((Relva,z):xs)))

-- | moverDireita3 : faz o caso em que y é diferente de 0 da função moverDireitaax
moverDireita3 :: Jogo -> Jogo
moverDireita3 (Jogo (Jogador (x,y)) (Mapa n ((t,lo):(Rio v,z):xs)))  | v<0 && elem x (posicaoTroncos z) && elem (x+v+1) ((posicaoTroncos (moverObs2 ((Rio v,z))))) = Jogo (Jogador (x+v+1,y)) (Mapa n (moverObs ((t,lo):(Rio v,z):xs)))
                                                                     | v>0 && elem x (posicaoTroncos z) && elem (x+v+1) ((posicaoTroncos (moverObs2 ((Rio v,z))))) = Jogo (Jogador (x+v+1,y)) (Mapa n (moverObs ((t,lo):(Rio v,z):xs)))
                                                                     | otherwise = (Jogo (Jogador (n+1,y)) (Mapa n (moverObs ((t,lo):(Rio v,z):xs)))) --o boneco esta morto se as condicoes de cima nao acontecerem logo colocamos o boneco fora do mapa para ser mais facil de dizer que ele está morto
moverDireita3 (Jogo (Jogador (x,y)) (Mapa n ((t,lo):(Estrada v,z):xs)))  | v/=0 && (elem x (posicaoCarros z) == False) && (elem (x+1) ((posicaoCarros (moverObsE2 x ((Estrada v,z))))) == False) = Jogo (Jogador (x+1,y)) (Mapa n (moverObsE (x+1) ((t,lo):(Estrada v,z):xs)))
                                                                         | otherwise = Jogo (Jogador ((-1),y)) (Mapa n (moverObsE (x+1) ((t,lo):(Estrada v,z):xs))) --o boneco esta morto se as condicoes de cima nao acontecerem logo colocamos o boneco fora do mapa para ser mais facil de dizer que ele está morto
moverDireita3 (Jogo (Jogador (x,y)) (Mapa n ((t,lo):(Relva,z):xs)))  | elem (x+1) (posicaoArvores z) = parado (Jogo (Jogador (x,y)) (Mapa n ((t,lo):(Relva,z):xs)))
                                                                     | otherwise = Jogo (Jogador (x+1,y)) (Mapa n (moverObs ((t,lo):(Relva,z):xs)))

-- | posicaoCarros : dá uma lista de inteiros com as posições dos carros numa estrada 
posicaoCarros :: [Obstaculo] -> [Int]
posicaoCarros (x:xs) = elemIndicess Carro (x:xs)
posicaoCarros [] = []

-- | moverObs2 : move os obstaculos e dá apenas a lista dos obstáculos movidos
moverObs2 :: (Terreno, [Obstaculo]) -> [Obstaculo]
moverObs2 (Estrada v, x) = if v<0 then deslesq2 v x
                           else if v > 0 then moverDir2 v x 
                           else x
moverObs2 (Rio v, x) = if v<0 then deslesq2 v x 
                       else if v>0 then moverDir2 v x 
                       else x 
moverObs2 (Relva, x ) = x 

-- | moverDir2 : auxiliar para moverObs2 que recebe uma lista e um inteiro (v) e move essa lista v unidades (para a direita)             
moverDir2 :: Int -> [a] -> [a]
moverDir2 v [] = []  
moverDir2 v (x:xs) = moverDir2ax (take v (moverDir2ax (x:xs))) ++ moverDir2ax (drop v (moverDir2ax (x:xs)))

-- | moveDir2ax : auxiliar da função moveDir2
moverDir2ax :: [a] -> [a] 
moverDir2ax [] = []
moverDir2ax (x:xs) = moverDir2ax xs ++ [x]

-- | ==Mover para a Esquerda

-- | moverEsquerda : função para quando a ação é Move Esquerda
moverEsquerda :: Jogo -> Jogo
moverEsquerda (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs))) = if x == 0 then parado (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))
                                                           else moverEsquerdaax (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))

-- | ===Auxiliares para moverEsquerda

-- | moverEsquerdaax : divide a função moverEsquerda nos casos em que y é igual a 0 e y é diferente de 0 
moverEsquerdaax :: Jogo -> Jogo
moverEsquerdaax (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs))) = if y == 0 then moverEsquerda2 (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))
                                                             else moverEsquerda3 (Jogo (Jogador (x,y)) (Mapa n ((t,z):xs)))

-- | moverEsquerda2 : faz o caso em que y é igual a 0
moverEsquerda2 :: Jogo -> Jogo
moverEsquerda2 (Jogo (Jogador (x,y)) (Mapa n ((Rio v,z):xs))) | v<0 && elem x (posicaoTroncos z) && elem (x+v-1) (posicaoTroncos (moverObs2 (Rio v,z))) = Jogo (Jogador (x+v-1,y)) (Mapa n (moverObs ((Rio v,z):xs)))
                                                              | v>0 && elem x (posicaoTroncos z) && elem (x+v-1) (posicaoTroncos (moverObs2 (Rio v,z))) = Jogo (Jogador (x+v-1,y)) (Mapa n (moverObs ((Rio v,z):xs)))
                                                              | otherwise = Jogo (Jogador ((-1),y)) (Mapa n (moverObs ((Rio v,z):xs))) --o boneco esta morto se as condicoes de cima nao acontecerem logo colocamos o boneco fora do mapa para ser mais facil de dizer que ele está morto
moverEsquerda2 (Jogo (Jogador (x,y)) (Mapa n ((Estrada v,z):xs))) | v/=0 && (elem x (posicaoCarros z) == False) && (elem (x-1) ((posicaoCarros (moverObsE2 x ((Estrada v,z))))) == False) = Jogo (Jogador (x-1,y)) (Mapa n (moverObsE (x-1) ((Estrada v,z):xs)))
                                                                  | otherwise = Jogo (Jogador ((-1),y)) (Mapa n (moverObsE (x-1) ((Estrada v,z):xs))) --o boneco esta morto se as condicoes de cima nao acontecerem logo colocamos o boneco fora do mapa para ser mais facil de dizer que ele está morto
moverEsquerda2 (Jogo (Jogador (x,y)) (Mapa n ((Relva,z):xs))) | elem (x-1) (posicaoArvores z) = parado (Jogo (Jogador (x,y)) (Mapa n ((Relva,z):xs)))
                                                              | otherwise = Jogo (Jogador (x-1,y)) (Mapa n (moverObs ((Relva,z):xs)))

-- | moverEsquerda3 : faz o caso em que y é diferente de 0
moverEsquerda3 :: Jogo -> Jogo
moverEsquerda3 (Jogo (Jogador (x,y)) (Mapa n ((t,lo):(Rio v,z):xs))) | v<0 && elem x (posicaoTroncos z) && elem (x+v-1) ((posicaoTroncos (moverObs2 ((Rio v,z))))) = Jogo (Jogador (x+v-1,y)) (Mapa n (moverObs ((t,lo):(Rio v,z):xs)))
                                                                     | v>0 && elem x (posicaoTroncos z) && elem (x+v-1) ((posicaoTroncos (moverObs2 ((Rio v,z))))) = Jogo (Jogador (x+v-1,y)) (Mapa n (moverObs ((t,lo):(Rio v,z):xs)))
                                                                     | otherwise = Jogo (Jogador ((-1),y)) (Mapa n (moverObs ((t,lo):(Rio v,z):xs))) --o boneco esta morto se as condicoes de cima nao acontecerem logo colocamos o boneco fora do mapa para ser mais facil de dizer que ele está morto
moverEsquerda3 (Jogo (Jogador (x,y)) (Mapa n ((t,lo):(Estrada v,z):xs))) | v/=0 && (elem x (posicaoCarros z) == False) && (elem (x-1) ((posicaoCarros (moverObsE2 x ((Estrada v,z))))) == False) = Jogo (Jogador (x-1,y)) (Mapa n (moverObsE (x-1) ((t,lo):(Estrada v,z):xs)))
                                                                         | otherwise = Jogo (Jogador ((-1),y)) (Mapa n (moverObsE (x-1) ((t,lo):(Estrada v,z):xs))) --o boneco esta morto se as condicoes de cima nao acontecerem logo colocamos o boneco fora do mapa para ser mais facil de dizer que ele está morto
moverEsquerda3 (Jogo (Jogador (x,y)) (Mapa n ((t,lo):(Relva,z):xs))) | elem (x-1) (posicaoArvores z) = parado (Jogo (Jogador (x,y)) (Mapa n ((t,lo):(Relva,z):xs)))
                                                                     | otherwise = Jogo (Jogador (x-1,y)) (Mapa n (moverObs ((t,lo):(Relva,z):xs)))

-- | ==Mais Funcoes auxiliares para Mover Jogador

-- | elemIndicess : calcula uma lista de posições em que um dado elemento ocorre numa lista
elemIndicess :: Eq a => a -> [a] -> [Int] 
elemIndicess x [] = []
elemIndicess x (y:ys) | x == y = 0 : map (+1) (elemIndicess x ys)
                      | otherwise = map (+1) (elemIndicess x ys)

-- | posicaoArvores : descobrir onde estao as arvores para poder impedir o boneco de andar se bater numa arvore na mesma linha
posicaoArvores :: [Obstaculo] -> [Int] 
posicaoArvores (x:xs) = elemIndicess Arvore (x:xs)
posicaoArvores [] = []

-- | move os obstaculos para a esquerda verificando se o jogador é atropelado
moverobsesq :: Int -> [Obstaculo] -> Int -> [Obstaculo]
moverobsesq v o x = if v == 0 || elem x (posicaoCarros o) then o
                    else moverobsesq (v-1) (drop 1 o ++ take 1 o) x

-- | move os obstaculos para a direita verificando se o jogador é atropelado
moverobsdir :: Int -> [Obstaculo] -> Int -> [Obstaculo]
moverobsdir v o x = if v == 0 || elem x (posicaoCarros o) then o 
                    else moverobsdir (v-1) (trocarOrdem (take 1 (trocarOrdem o)) ++ trocarOrdem (drop 1 (trocarOrdem o))) x

-- | função que mexe os obstaculos da estrada para ver se o jogador é atropelado
moverObsE :: Int -> [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]   
moverObsE x [] = []
moverObsE x ((Rio v, y:ys):b) = if v > 0 then ((Rio v, (desldir v (y:ys)))):moverObsE x b 
                                 else if v < 0 then ((Rio v, (deslesq2 v (y:ys)))):moverObsE x b 
                                 else ((Rio v, y:ys)):moverObsE x b 
moverObsE x ((Estrada v, y:ys):b) = if v > 0 then ((Estrada v, (moverobsdir v (y:ys) x))):moverObsE x b 
                                 else if v < 0 then ((Estrada v, (moverobsesq (abs v) (y:ys) x))):moverObsE x b 
                                 else ((Estrada v, y:ys)):moverObsE x b 
moverObsE x ((Relva, z):b) = (Relva, z):moverObsE x b 

-- | função que mexe os obstaculos da estrada mas só dá como resultado a lista de obstaculos, para poder verificar se vai haver um carro na posição para onde o jogador vai
moverObsE2 :: Int -> (Terreno,[Obstaculo]) -> [Obstaculo]
moverObsE2 x (Rio v, y:ys) = if v > 0 then (desldir v (y:ys))
                                 else if v < 0 then (deslesq2 v (y:ys))
                                 else y:ys
moverObsE2 x (Estrada v, y:ys) = if v > 0 then moverobsdir v (y:ys) x
                                 else if v < 0 then moverobsesq (abs v) (y:ys) x 
                                 else y:ys
moverObsE2 x (Relva, z) = z 