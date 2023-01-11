module Tarefa6_2022li1g044 where 


import LI12223 
import Tarefa1_2022li1g044
import Tarefa2_2022li1g044
import Tarefa3_2022li1g044
import Tarefa4_2022li1g044
import Tarefa5_2022li1g044
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
--import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.Exit
import System.Directory
import System.Posix (fileExist)
import Data.Char 
import Text.Read (readMaybe, Lexeme (String))



mapa = Jogo (Jogador (4,4)) (Mapa 9 [(Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),
                                    (Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum]),
                                    (Estrada 2,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),
                                    (Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),
                                    (Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum]),
                                    (Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),
                                    (Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum]),
                                    (Estrada 2,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),
                                    (Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),
                                    (Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum])])

rect = Scale 1.1 1.1 $ Translate 395 408 $ Polygon [(-90,-75),(90,-75),(90,75),(-90,75),(-90,-75)]

-- | desenhaObs : Função que desenha cada os obstaculos de cada linha, quando as velocidades sdos terrenos ão positivas
desenhaObs :: [Obstaculo] -> Images -> Picture
desenhaObs o i = case o of 
    (Arvore:h)                             -> Pictures [Translate (-800) 0 (i !! 7),Translate 200 0 (desenhaObs h i)]
    (Carro:Carro:Carro:h)                  -> Pictures [Translate (-800) 0 $ Scale 3 1.7 (i !! 5),Translate 800 0 (desenhaObs h i)]
    (Carro:Carro:h)                        -> Pictures [Translate (-800) 0 (i !! 5),Translate 400 0 (desenhaObs h i)]
    (Carro:h)                              -> Pictures [Translate (-800) 0 (i !! 6),Translate 200 0 (desenhaObs h i)]
    Tronco:Tronco:Tronco:Tronco:Tronco:h:s -> Pictures [Scale 3.5 1 $ Translate (-800) 0 (i !! 9),Translate 1000 0 (desenhaObsNeg (h:s) i)]
    Tronco:Tronco:Tronco:Tronco:h:s        -> Pictures [Scale 2.8 1 $ Translate (-800) 0 (i !! 9),Translate 800 0 (desenhaObsNeg (h:s) i)]
    Tronco:Tronco:Tronco:h:s               -> Pictures [Scale 2.1 1 $ Translate (-800) 0 (i !! 9),Translate 600 0 (desenhaObsNeg (h:s) i)]
    Tronco:Tronco:h:s                      -> Pictures [Scale 1.4 1 $ Translate (-800) 0 (i !! 8),Translate 400 0 (desenhaObsNeg (h:s) i)]
    (Tronco:h)                             -> Pictures [Translate (-800) 0 (i !! 8),Translate 200 0 (desenhaObs h i)]
    (Nenhum:h)                             -> Pictures [Translate (-800) 0 Blank,Translate 200 0 (desenhaObs h i)]
    [] -> Blank

-- | desenhaObsNeg : Analogamente à função anterior, gera os obstaculos cujos terrenos tenham velocidades negativas
desenhaObsNeg :: [Obstaculo] -> Images -> Picture
desenhaObsNeg o i = case o of 
    Arvore:h                               -> Pictures [Translate (-800) 0 (i !! 7),Translate 200 0 (desenhaObsNeg h i)]
    Carro:Carro:Carro:h:s                  -> Pictures [Translate (-800) 0 $ Rotate 180 $ Scale 3.3 1.7 (i !! 5),Translate 800 0 (desenhaObsNeg (h:s) i)]
    Carro:Carro:h:s                        -> Pictures [Translate (-800) 0 $ Rotate 180 (i !! 5),Translate 400 0 (desenhaObsNeg (h:s) i)]
    Carro:h                                -> Pictures [Translate (-800) 0 $ Rotate 180 (i !! 4),Translate 200 0 (desenhaObsNeg h i)]
    Tronco:Tronco:Tronco:Tronco:Tronco:h:s -> Pictures [Scale 3.5 1 $ Translate (-800) 0 $ Rotate 180 (i !! 9),Translate 1000 0 (desenhaObsNeg (h:s) i)]
    Tronco:Tronco:Tronco:Tronco:h:s        -> Pictures [Scale 2.8 1 $ Translate (-800) 0 $ Rotate 180 (i !! 9),Translate 800 0 (desenhaObsNeg (h:s) i)]
    Tronco:Tronco:Tronco:h:s               -> Pictures [Scale 2.1 1 $ Translate (-800) 0 $ Rotate 180 (i !! 9),Translate 600 0 (desenhaObsNeg (h:s) i)]
    Tronco:Tronco:h:s                      -> Pictures [Scale 1.4 1 $ Translate (-800) 0 $ Rotate 180 (i !! 8),Translate 400 0 (desenhaObsNeg (h:s) i)]
    Tronco:h                               -> Pictures [Translate (-800) 0 $ Rotate 180 (i !! 8),Translate 200 0 (desenhaObsNeg h i)]
    Nenhum:h                               -> Pictures [Translate (-800) 0 Blank,Translate 200 0 (desenhaObsNeg h i)]
    [] -> Blank

-- | desenhaMapa: Desenha as imagens dos terrenos com os obstaculos nos respetivos lugares
desenhaMapa :: Jogo -> Images -> Picture
desenhaMapa (Jogo (Jogador c) (Mapa n ((t,o):xs))) i = case t of 
    Relva -> Pictures [Pictures [head i,desenhaObs o i],Translate 0 (-120) (desenhaMapa (Jogo (Jogador c) (Mapa n xs)) i)]
    Estrada v -> if v > 0 then Pictures [Pictures [estt,desenhaObs o i],Translate 0 (-120) (desenhaMapa (Jogo (Jogador c) (Mapa n xs)) i)] 
                          else Pictures [Pictures [estt,desenhaObsNeg o i],Translate 0 (-120) (desenhaMapa (Jogo (Jogador c) (Mapa n xs)) i)] 
                            where estt = if estrd (Mapa n ((t,o):xs)) then i !! 3 else i !! 2
    Rio v -> if v > 0 then Pictures [Pictures [i !! 1,desenhaObs o i],Translate 0 (-120) (desenhaMapa (Jogo (Jogador c) (Mapa n xs)) i)]
                      else Pictures [Pictures [i !! 1,desenhaObsNeg o i],Translate 0 (-120) (desenhaMapa (Jogo (Jogador c) (Mapa n xs)) i)]

desenhaMapa _ _ = Blank

-- | Função auxiliar para determinar se uma estrada é predecedida por outra estrada ou não, para qual das imagens de estrada desenhar
estrd :: Mapa -> Bool
estrd (Mapa _ ((Estrada _,o):(t,os):xs)) = case t of 
    Estrada _ -> True 
    _ -> False 
estrd _ = True 



-- | desenhaJogador: Função que transforma o Jogador numa imagem e a move segundo as coordenadas do jogador
desenhaJogador :: World -> Picture
desenhaJogador (m, Jogo (Jogador (x,y)) _,j,_,_,ij,n) = case m of 
    ModoJogo Homem -> Translate (200*fromIntegral x) ((-120)*fromIntegral y) (head ij)
    ModoJogo Default -> Translate (200*fromIntegral x) ((-120)*fromIntegral y) (ij !! 1)
    _ -> Blank

-- | desenhaJogo: Função que acerta as posições das imagens do Mapa e do Jogador no ecrã do jogo
desenhaJogo :: World -> Picture
desenhaJogo (m,p,j,r,i,ij,ti) = Translate 0 480 $ Pictures [desenhaMapa p i,Translate (-800) 0 $ desenhaJogador (m,p,j,r,i,ij,ti)] 

-- | Função apenas utilizada para dar o estado inicial do jogo, que será utilizada diretamente na função play do module Main
estadoInicial :: (EstadoJogo,Jogo,Jogada,Time)-> [Int] -> Images -> Images -> World
estadoInicial (s,p,j,t) r i ij = (Opcoes Jogar,mapa,Parado,r,i,ij,0) 

-- | desenhaWorld : Aplica as funções auxiliares de desenho do jogo, filtrando qual delas aplicar através do estado de Jogo desejado
desenhaWorld :: World -> IO Picture 
desenhaWorld (m,p,j,l,i,ij,t) = case m of  
    ModoJogo _-> do return $ Pictures [desenhaJogo (m,animaJogo p j,j,l,i,ij,t),Translate 390 (-20) rect,Translate 390 (-20) $ desTempo t]
        where desTempo :: Float -> Picture 
              desTempo t = if t < 10 then Color white $ Translate 400 400 $ Scale 0.9 0.9 $ Text $ show (round t) else Color white $ Translate 375 400 $ Scale 0.9 0.9 $ Text $ show (round t)
    Save Jogar _ -> do return $ ij !! 8
    Save Sair  _ -> do return $ ij !! 9
    ModoVenceu Sair-> do return $ ij !! 7
    ModoVenceu Jogar -> do return $ ij !! 6
    Opcoes Jogar -> do return $ ij !! 2
    Opcoes Sair -> do return $ ij !! 3
    Escolhe Default -> do return $ ij !! 4
    Escolhe Homem -> do return $ ij !! 5


-- | reageEvento : Controla o que acontece ao World quando uma tecla é pressionada, de acordo com o modo de jogo
reageEvento :: Event -> World -> IO World
reageEvento (EventKey (Char 'w') Down _ _) (ModoJogo s,Jogo (Jogador (x,y)) m,_,r,i,ij,n) = do -- ^ Quando a tecla W pressionada, o jogador move-se para a cima
    return (ModoJogo s,animaJogo (Jogo (Jogador (x,y)) m) (Move Cima),Parado,r,i,ij,n) 

reageEvento (EventKey (Char 'a') Down _ _) (ModoJogo s,Jogo (Jogador (x,y)) m,_,r,i,ij,n) = do -- ^ Quando a tecla A pressionada, o jogador move-se para a esquerda
    return (ModoJogo s,animaJogo (Jogo (Jogador (x,y)) m) (Move Esquerda),Parado,r,i,ij,n) 

reageEvento (EventKey (Char 's') Down _ _) (ModoJogo s,Jogo (Jogador (x,y)) m,_,r,i,ij,n) = do -- ^ Quando a tecla S pressionada, o jogador move-se para a baixo
    return (ModoJogo s,animaJogo (Jogo (Jogador (x,y)) m) (Move Baixo),Parado,r,i,ij,n) 

reageEvento (EventKey (Char 'd') Down _ _) (ModoJogo s,Jogo (Jogador (x,y)) m,_,r,i,ij,n) = do -- ^ Quando a tecla D pressionada, o jogador move-se para a direita
    return (ModoJogo s,animaJogo (Jogo (Jogador (x,y)) m) (Move Direita),Parado,r,i,ij,n) 


reageEvento (EventKey (Char 'q') Down _ _) (ModoJogo s,p,j,r,i,ij,t) = do writeFile "save.txt" (show (ModoJogo s,p,j,t)) -- ^ Pressionando a tecla Q, o jogo entra em pausa, guardando informações do jogo
                                                                          return (Save Jogar s,p,j,r,i,ij,t) 
reageEvento (EventKey (Char 'q') Down _ _) (Save jo s,p,j,r,i,ij,t) = do return (ModoJogo s,p,j,r,i,ij,t)
reageEvento (EventKey (Char 'w') Down _ _) (Save Jogar s,p,j,r,i,ij,t) = do return (Save Sair s,p,j,r,i,ij,t) -- ^ Navegação no menu de pausa
reageEvento (EventKey (Char 'w') Down _ _) (Save Sair s,p,j,r,i,ij,t) = do return (Save Jogar s,p,j,r,i,ij,t) -- ^ Navegação no menu de pausa
reageEvento (EventKey (Char 's') Down _ _) (Save Jogar s,p,j,r,i,ij,t) = do return (Save Sair s,p,j,r,i,ij,t) -- ^ Navegação no menu de pausa
reageEvento (EventKey (Char 's') Down _ _) (Save Sair s,p,j,r,i,ij,t) = do return (Save Jogar s,p,j,r,i,ij,t) -- ^ Navegação no menu de pausa
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Save Jogar s,p,j,r,i,ij,t) = do return (ModoJogo s,p,j,r,i,ij,t) -- ^ Escolha de Voltar ao Jogo, apartir do mesmo ponto
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Save Sair s,p,j,r,i,ij,t) = do writeFile "save.txt" (show (ModoJogo s,p,j,t)) -- ^ Volta ao Menu Inicial, podendo trocar a personagem, sem alterar a posição nem a pontuação
                                                                                      return (Opcoes Jogar,p,j,r,i,ij,t)

reageEvento (EventKey (Char 'w') Down _ _) (Opcoes Jogar,p,j,r,i,ij,t) = do return (Opcoes Sair,p,j,r,i,ij,t) -- ^ Navegação no menu
reageEvento (EventKey (Char 's') Down _ _) (Opcoes Jogar,p,j,r,i,ij,t) = do return (Opcoes Sair,p,j,r,i,ij,t) -- ^ Navegação no menu 
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar,p,j,r,i,ij,t) = do return (Escolhe Default,p,j,r,i,ij,t) -- ^ Escolha da opção do menu

reageEvento (EventKey (Char 'd') Down _ _) (Escolhe Default,p,j,r,i,ij,t) = do return (Escolhe Homem,p,j,r,i,ij,t) -- ^ Navegação na escolha do personagem 
reageEvento (EventKey (Char 'a') Down _ _) (Escolhe Default,p,j,r,i,ij,t) = do return (Escolhe Homem,p,j,r,i,ij,t) -- ^ Navegação na escolha do personagem 
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Escolhe Default,p,j,r,i,ij,t) = do return (ModoJogo Default,p,j,r,i,ij,t) -- ^ Escolha do personagem Default

reageEvento (EventKey (Char 'd') Down _ _) (Escolhe Homem,p,j,r,i,ij,t) = do return (Escolhe Default,p,j,r,i,ij,t)-- ^ Navegação na escolha do personagem
reageEvento (EventKey (Char 'a') Down _ _) (Escolhe Homem,p,j,r,i,ij,t) = do return (Escolhe Default,p,j,r,i,ij,t)-- ^ Navegação na escolha do personagem
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Escolhe Homem,p,j,r,i,ij,t) = do return (ModoJogo Homem,p,j,r,i,ij,t) -- ^ Escolha do personagem Homem


reageEvento (EventKey (Char 'w') Down _ _) (Opcoes Sair,p,j,r,i,ij,t) = do return (Opcoes Jogar,p,j,r,i,ij,t) -- ^ Navegação no menu inicial
reageEvento (EventKey (Char 's') Down _ _) (Opcoes Sair,p,j,r,i,ij,t) = do return (Opcoes Jogar,p,j,r,i,ij,t) -- ^ Navegação no menu inicial
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair,p,j,_,_,_,t) = do putStrLn "Fim" -- ^ Mensagem de final de jogo
                                                                                     exitSuccess -- ^ Saida do Jogo

reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ModoVenceu Jogar ,p,j,r,i,ij,t) = do return $ estadoInicial (Opcoes Jogar,p,j,t) r i ij -- ^ Volta ao menu inicial
reageEvento (EventKey (Char 'd') Down _ _) (ModoVenceu Jogar ,p,j,r,i,ij,t) = do return (ModoVenceu Sair,p,j,r,i,ij,t) -- ^ Navegação no menu de final de jogo
reageEvento (EventKey (Char 'a') Down _ _) (ModoVenceu Sair ,p,j,r,i,ij,t) = do return (ModoVenceu Jogar,p,j,r,i,ij,t) -- ^ Navegação no menu de final de jogo
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ModoVenceu Sair,_,_,_,_,_,_) = do exitSuccess -- ^ Mensagem de final de jogo

reageEvento _ (m,p,j,r,i,ij,t) = do return (m,p,Parado,r,i,ij,t) -- ^ Qualquer outra tecla que seja pressionada, o mundo permanece igual e o jogador permanece parado

-- | reageTempo : Determina as mudanças no World a cada atualização do jogo / repetição das funções de acordo com o novo World
reageTempo :: Float -> World -> IO World
reageTempo a (ModoJogo s,Jogo (Jogador (x,y)) m,j,h:hs,i,ij,t) 
    | jogoTerminou (Jogo (Jogador (x,y)) m) = do return (ModoVenceu Jogar,Jogo (Jogador (x,y)) m,j,h:hs,i,ij,t)
    | otherwise = do return (ModoJogo s,animaJogo (deslizaJogo h (Jogo (Jogador (x,y+1)) m)) j,j,hs,i,ij,t+a)
reageTempo n (Save o s,p,j,r,i,ij,t) = do return (Save o s,p,j,r,i,ij,t)
reageTempo n (ModoVenceu a,p,j,r,i,ij,t) = do return (ModoVenceu a,p,j,r,i,ij,t)
reageTempo n (m,p,j,r,i,ij,t) = do return (m,p,j,r,i,ij,t)

-- | fr : Definição da quantidade de vezes que o World será atualizado por segundo
fr :: Int 
fr = 1

-- | background : Definicão da cor de fundo do jogo
background :: Color
background = greyN 0.8

-- | window : Definição dos parâmetros da janela de jogo
window :: Display
window = FullScreen

-- | rNum : Função gera uma lista infinita de números aleatórios de 0 a 100 para a aleatoriedade das funções 
rNum :: StdGen -> [Int]
rNum gen =
            let (firstNum, newGen) = random gen
            in  (mod firstNum 100 : rNum newGen)

-- | readEstado : Função que trata dos dados guardados
readEstado :: IO (EstadoJogo,Jogo,Jogada,Time)
readEstado = do 
    fileExist <- doesFileExist "save.txt"
    saved <- if fileExist then readFile "save.txt" 
                          else return "(Opcoes Jogar ,Jogo (Jogador (4,4)) (Mapa 9 [(Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum]),(Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum])]),Parado,0)"
    return (read saved :: (EstadoJogo,Jogo,Jogada,Time))