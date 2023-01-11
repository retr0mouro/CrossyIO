module Main where 

import LI12223
import Tarefa1_2022li1g044
import Tarefa2_2022li1g044
import Tarefa3_2022li1g044
import Tarefa4_2022li1g044
import Tarefa5_2022li1g044
import Tarefa6_2022li1g044
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
--import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.Exit
import System.Directory

main :: IO ()
main = do carY <- loadBMP "img/CarroY.bmp"
          cdp <- loadBMP "img/CdP.bmp"  
          carG <- loadBMP "img/CarroG.bmp"
          est1 <- loadBMP "img/est1.bmp"
          est2 <- loadBMP "img/est2.bmp"
          rel <- loadBMP "img/rel.bmp"
          riv <- loadBMP "img/riv.bmp"
          arv <- loadBMP "img/arv.bmp"
          ze1 <- loadBMP "img/ze1.bmp"
          ze2 <- loadBMP "img/ze2.bmp"
          deft <- loadBMP "img/dfj.bmp"
          mjogar <- loadBMP "img/mjogar.bmp"
          mSair <- loadBMP "img/mSair.bmp" 
          pd <- loadBMP "img/default.bmp"
          ps <- loadBMP "img/skin.bmp"
          gOSair <- loadBMP "img/GOs.bmp"
          gOpag <- loadBMP "img/GOpag.bmp"
          log <- loadBMP "img/log.bmp"
          log2 <- loadBMP "img/log2.bmp"
          pauseV <- loadBMP "img/pauseV.bmp"
          pauseS <- loadBMP "img/pauseS.bmp"
          let img = [Scale 2 1 rel,Scale 2 1 riv,Scale 2 1 est1,Scale 2 1 est2,carG,cdp,carY,Translate 0 20 $ Scale 0.7 1 arv,Scale 1.4 0.7 log,log2]
          let jog = [ze1,deft,mjogar,mSair,pd,ps,gOpag,gOSair,pauseV,pauseS]
          (s,p,j,t) <- readEstado 
          playIO 
            window 
            background 
            fr
            (estadoInicial (s,p,j,t) (rNum (mkStdGen 99)) img jog) 
            desenhaWorld
            reageEvento
            reageTempo