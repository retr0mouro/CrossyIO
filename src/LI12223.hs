{- |
Module      : LI12223
Description : Módulo auxiliar para LI1 22/23.
Copyright   : Manuel Barros <d13242@di.uminho.pt>
              Nelson Estevão <d12733@di.uminho.pt>
              Olga Pacheco <omp@di.uminho.pt>
              Xavier Pinho <d12736@di.uminho.pt>

Tipos de dados e funções auxiliares para a realização do projeto de LI1 em 2022/23.
 -}
module LI12223 (
  -- * Tipos de dados
  -- ** Básicos
  Coordenadas , Largura , Velocidade,
  -- ** Mapas
  Mapa(..), Terreno(..), Obstaculo(..),
    -- ** Jogo
  Jogo(..), Jogador(..), Direcao(..), Jogada(..),
  -- ** Parte gráfica
  Skin(..), Opcao(..), EstadoJogo(..), World(..), Images(..), Time
  ) where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game

-- | Velocidade que irá afetar a movimentação dos 'Obstaculo's de um 'Mapa'.
type Velocidade = Int

{- | Cada linha de um 'Mapa' é um Terreno em particular contendo 'Obstaculo's.

As linhas do tipo 'Rio' ou 'Estrada' têm a propriedade 'Velocidade' que indicam a velocidade de deslocamento dos obstáculos. Podendo esta ser negativa, indicando assim a sua direção.
-}
data Terreno
  = Rio Velocidade
  | Estrada Velocidade
  | Relva
  deriving (Show, Read, Eq)

-- | Um Obstáculo numa linha de um 'Mapa'.
data Obstaculo
  = Nenhum -- ^ a ausência de obstáculos
  | Tronco -- ^ os troncos deslizam apenas em 'Rio'
  | Carro -- ^ os carros movimentam-se apenas em 'Estrada'
  | Arvore -- ^ as árvores são um obstáculo fixo que não se move e apenas são possíveis em 'Relva'
  deriving (Show, Read, Eq)

-- | Comprimento de um 'Mapa'.
type Largura = Int

-- | O Mapa que constituí o 'Jogo'.
data Mapa =
  Mapa Largura [(Terreno, [Obstaculo])]
  deriving (Show, Read, Eq)

-- | Par de coordenadas de uma posição no 'Mapa'.
type Coordenadas = (Int, Int)

-- | O Jogador define o personagem controlado no 'Jogo'.
newtype Jogador =
  Jogador Coordenadas
  deriving (Show, Read, Eq)

-- | Definição base de um jogo.
data Jogo =
  Jogo
    Jogador -- ^ o personagem do jogo
    Mapa -- ^ o mapa em que se está a jogar
  deriving (Show, Read, Eq)

-- | Direção de uma 'Jogada' feita por um 'Jogador' no 'Mapa'.
data Direcao
  = Cima
  | Baixo
  | Esquerda
  | Direita
  deriving (Show, Read, Eq)

-- | As acções que podem ser tomadas pelo 'Jogador' em cada estado do 'Jogo'.
data Jogada
  = Parado -- ^ tipo que define a ausência de uma acção do 'Jogador'
  | Move Direcao -- ^ um movimento do jogador numa determinada 'Direcao'
  deriving (Show, Read, Eq)

-- | Escolha da personagem
data Skin = Default -- ^ Galinha
          | Homem -- ^ Homem
          deriving (Show,Read,Eq)
-- | Escolha para iniciar o jogo ou para sair
data Opcao = Jogar -- ^ Iniciar o jogo
           | Sair  -- ^ Sair do jogo
           deriving (Show,Read,Eq)
-- | Ecrãs para todos os modos de jogo
data EstadoJogo = Opcoes Opcao     -- ^ Ecrã inicial
                | Escolhe Skin     -- ^ Ecrã de escolha de personagem
                | ModoJogo Skin    -- ^ Ecrã jogo, com a skin escolhida
                | Save Opcao Skin
                | ModoVenceu Opcao -- ^ Ecrã de final de Jogo
                deriving (Show,Read,Eq)
-- | Agrupamento de informações para obtenção de imagens do jogo 
type World = (EstadoJogo,Jogo,Jogada,[Int],Images,Images,Time)


-- | Definição do tipo das imagens externas 
type Images = [Picture]

-- | Definição de uma outra variável de tempo para dar movimento à personagem Homem
type Time = Float 
