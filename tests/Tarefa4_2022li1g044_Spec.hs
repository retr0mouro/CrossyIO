module Tarefa4_2022li1g044_Spec where

import LI12223
import Tarefa4_2022li1g044
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Teste 1" ~: False ~=? jogoTerminou  (Jogo (Jogador (1,0)) (Mapa 6 [(Relva , [Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-4), [Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]),(Estrada (-4), [Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum])])),
                                              "Teste 2" ~: False ~=? jogoTerminou  (Jogo (Jogador (2,1)) (Mapa 6 [(Relva , [Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2, [Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]),(Estrada 2, [Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum])])),
                                              "Teste 3" ~: False ~=? jogoTerminou  (Jogo (Jogador (1,2)) (Mapa 6 [(Relva , [Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2, [Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]),(Estrada 2, [Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum])])),
                                              "Teste 4" ~: True ~=? jogoTerminou (Jogo (Jogador (4,1)) (Mapa 6 [(Relva , [Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2, [Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]),(Estrada 4, [Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum])])),
                                              "Teste 5" ~: True ~=? jogoTerminou (Jogo (Jogador (4,2)) (Mapa 6 [(Relva , [Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio 4, [Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]),(Estrada 2, [Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum])])),
                                              "Teste 6" ~: True ~=? jogoTerminou (Jogo (Jogador (0,3)) (Mapa 6 [(Relva , [Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio 3, [Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]),(Estrada 3, [Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum])])),
                                              "Teste 7" ~: True ~=? jogoTerminou (Jogo (Jogador ((-1),2)) (Mapa 6 [(Relva , [Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-2), [Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]),(Estrada 2, [Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum])])),
                                              "Teste 8" ~: True ~=? jogoTerminou (Jogo (Jogador (7,4)) (Mapa 6 [(Relva , [Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2, [Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]),(Estrada 2, [Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum])])),
                                              "Teste 9" ~: False ~=? jogoTerminou (Jogo (Jogador (3,4)) (Mapa 6 [(Relva , [Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2, [Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]),(Estrada 2, [Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum]),(Relva , [Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2, [Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]),(Estrada 2, [Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum])])),
                                              "Teste 10" ~: True ~=? jogoTerminou (Jogo (Jogador (3,5)) (Mapa 6 [(Relva , [Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2, [Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]),(Estrada 2, [Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum]),(Relva , [Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio 2, [Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]),(Estrada 2, [Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum])]))
                                             ]

