module Main where 

import LI12223
import Tarefa1_2022li1g044
import Tarefa2_2022li1g044
import Tarefa3_2022li1g044
import Tarefa4_2022li1g044
import Tarefa5_2022li1g044
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import System.Random

{-type World = (Jogo,Player,[Int],Time)

type Player = [Picture]

type Time = Float -}

{-}
circulo1 :: Picture
circulo1 = color white $ Translate (69) (-69) (Circle 50) 

circulo2 :: Picture 
circulo2 = rotate (-45) $ scale 0.5 1 $ Translate (-60) 30 circulo1 

circulo3 :: Picture
circulo3 = scale 1 0.5 $ circleSolid 20

quadradoVerde :: Picture
quadradoVerde = color white $ Translate 400 400 (rectangleSolid 20 20) 

linhaPoligonal :: Picture
linhaPoligonal = color red $ (Line [(0,0), (-200,0), (200,200), (0,200), (0,0)])

circulo = Pictures [circulo1,circulo2,circulo3,quadradoVerde,linhaPoligonal]
-}

--mapa = (Mapa 8 [(Relva , [Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),(Rio (-4), [Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum,Nenhum]),(Estrada (-4), [Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum,Carro,Nenhum])])--(Mapa 7 [(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum]),(Rio 3,[Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco]),
         --           (Estrada 4,[Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro])])
mapa = (Jogo (Jogador (0,0)) (Mapa 7 [(Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum]),(Estrada 2,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro]),(Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum]),(Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum]),(Estrada 2,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro]),(Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum])]))

relva = ((Color green) $ (Polygon [(-50000,-60),(50000,-60),(50000,60),(-50000,60),(-50000,-60)]))
estrada = ((Color (greyN 0.1)) $ (Polygon [(-50000,-60),(50000,-60),(50000,60),(-50000,60),(-50000,-60)]))
--r = rio 
rio = (Color ((bright (light blue))) $ (Polygon [(-50000,-60),(50000,-60),(50000,60),(-50000,60),(-50000,-60)]))
arvore = ((Color (dark green))$ (Polygon [(-25,-25),(25,-25),(25,25),(-25,25),(-25,-25)]))
carro = ((Color red) $ Pictures [Polygon [(-50,-15),(50,-15),(50,15),(-50,15),(-50,-15)],Polygon [(-15,-10),(5,-10),(5,10),(-5,10),(-5,-10)]])
tronco = ((Color (addColors blue orange)) $ Polygon [(-50, -25),(50,-25),(50,25),(-50,25),(-50,-25)])
nenhum = ((Color yellow) $ Circle 25)
jogador = (Pictures [Color white $ (Polygon [(-25,-25),(25,-25),(25,25),(-25,25),(-25,-25)]),(Line [(0,5),(0,(-5))])])


desenhaObs :: [Obstaculo] -> Picture
desenhaObs (Arvore:h) = Pictures [(Translate (-600) (0) arvore),(Translate 200 0 (desenhaObs h))]
desenhaObs (Carro:h) = Pictures [(Translate (-600) (0) carro),(Translate 200 0 (desenhaObs h))]
desenhaObs (Tronco:h) = Pictures [(Translate (-600) (0) tronco),(Translate 200 0 (desenhaObs h))]
desenhaObs (Nenhum:h) = Pictures [(Translate (-600) (0) nenhum),(Translate 200 0 (desenhaObs h))]
desenhaObs [] = Circle 15


desenhaMapa :: Jogo -> Picture
desenhaMapa (Jogo (Jogador c) (Mapa n ((Relva,o):xs))) = Pictures [(Pictures [relva,desenhaObs o]),(Translate 0 (-120) (desenhaMapa (Jogo (Jogador c) (Mapa n xs))))]
desenhaMapa (Jogo (Jogador c) (Mapa n ((Estrada _,o):xs))) = Pictures [(Pictures [estrada,desenhaObs o]),(Translate 0 (-120) (desenhaMapa (Jogo (Jogador c) ((Mapa n xs)))))]
desenhaMapa (Jogo (Jogador c) (Mapa n ((Rio _,o):xs))) = Pictures [(Pictures [rio,desenhaObs o]),(Translate 0 (-120) (desenhaMapa (Jogo (Jogador c) (Mapa n xs))))]
desenhaMapa _ = Circle 15

{-obsdesenha :: Picture -> [Obstaculo]
obsdesenha (Pictures (h:t)) = case h of 
    arvore -> (Arvore : (obsdesenha (Pictures t))) 
    carro -> (Carro : (obsdesenha (Pictures t)))
    tronco -> (Tronco : (obsdesenha (Pictures t)))

terdesenha :: Picture -> Terreno
terdesenha t = case t of 
    rio -> 

mapadesenha :: Picture -> Mapa 
mapadesenha (Pictures (t:o)) = case t of
    relva -> (Mapa ((length o) - 1) [(Relva,(obsdesenha (Pictures (take ((length o) - 1) o))))])
    estrada -> (Mapa ((length o) - 1) [(Estrada 0,(obsdesenha (Pictures (take ((length o) - 1) o))))])
    rio -> (Mapa ((length o) - 1) [(Rio 0,(obsdesenha (Pictures (take ((length o) - 1) o))))])
    _ -> (Mapa ((length o) - 1) [(mapadesenha (Pictures o),(t:(obsdesenha (Pictures (take ((length o) - 1) o)))))])-}



desenhaJogador :: World-> Picture
desenhaJogador ((Jogo (Jogador (x,y)) _),_,t) = Translate (fromIntegral x) (fromIntegral y) (jogador) 

desenhaJogo :: World -> Picture
desenhaJogo (j,r,ti) = Translate 0 480 $ Pictures [(desenhaMapa j),(Translate 0 (-480) $ desenhaJogador (j,r,ti))] 

estadoInicial :: [Int] -> World
estadoInicial l = (mapa,l,0) 
    
desenhaWorld :: World -> Picture 
desenhaWorld w = (desenhaJogo w) 

--(Translate 0 450) . (desenhaMJ)
{-andar :: Event -> J -> Picture
andar (reageEvento (EventKey (Char 'w') Down _ _) (Jogador (x,y))) = Translate 0 (120) jogador
andar (reageEvento (EventKey (Char 'a') Down _ _) (Jogador (x,y))) = Translate (-120) (0) jogador
andar (reageEvento (EventKey (Char 's') Down _ _) (Jogador (x,y))) = Translate 0 (-120) jogador
andar (reageEvento (EventKey (Char 'd') Down _ _) (Jogador (x,y))) = Translate 0 (120) jogador-}

reageEvento :: Event -> World -> World
reageEvento (EventKey (Char 'w') Down _ _) ((Jogo (Jogador (x,y)) m),r,n) = ((Jogo (Jogador (x,y+120)) m),r,n)
reageEvento (EventKey (Char 'a') Down _ _) ((Jogo (Jogador (x,y)) m),r,n) = ((Jogo (Jogador (x-200,y)) m),r,n)
reageEvento (EventKey (Char 's') Down _ _) ((Jogo (Jogador (x,y)) m),r,n) = ((Jogo (Jogador (x,y-120)) m),r,n)
reageEvento (EventKey (Char 'd') Down _ _) ((Jogo (Jogador (x,y)) m),r,n) = ((Jogo (Jogador (x+200,y)) m),r,n)
reageEvento _ s = s

reageTempo :: Float -> World -> World
reageTempo (a) ((Jogo (Jogador (x,y)) (Mapa n l)),(h:hs),t)= (((deslizaJogo h (Jogo (Jogador (x,y-120)) (Mapa n l)))),hs,t)--(Jogo (Jogador (x,y-120)) (estendeMapa {-(Jogo (Jogador (x,y-120))-} (Mapa n l) 57))--(mapadesenha (Translate 0 (-10) ( desenhaMapa (Jogo (Jogador (x,y)) (Mapa n l)))))) (33))
--reageTempo n (Jogador (x,y)) = Jogador (x,y)

fr :: Int 
fr = 1

background :: Color
background = greyN 0.8

window :: Display
window = FullScreen

main :: IO ()
main = do --rio <- loadBMP "img/agua.bmp"
          --ze1 <- loadBMP "img/zef1.bmp" 
          --ze2 <- loadBMP "img/zef2.bmp" 
          --let jose = [scale 0.01 0.01 ze1, scale 0.01 0.01 ze2]
          play 
            window 
            background 
            fr
            (estadoInicial (randNum (mkStdGen 2))) 
            desenhaWorld
            reageEvento
            reageTempo