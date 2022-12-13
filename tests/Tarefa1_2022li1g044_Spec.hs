module Tarefa1_2022li1g044_Spec where

import LI12223
import Tarefa1_2022li1g044
import Test.HUnit

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test ["Teste 1" ~: False ~=? (mapaValido (Mapa 12 [(Relva,[Arvore,Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore]),(Relva,[Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum])])),
                                              "Teste 2" ~: True ~=? (mapaValido (Mapa 12 [(Relva,[Arvore,Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum])])),
                                              "Teste 3" ~: True ~=? (mapaValido (Mapa 12 [(Estrada 3,[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-3),[Carro,Nenhum,Carro,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),(Rio (-5),[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),(Rio 2,[Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco]),(Estrada 3,[Nenhum,Carro,Carro,Carro,Nenhum,Carro,Carro,Carro,Nenhum,Carro,Carro,Carro]),(Estrada (-3),[Nenhum,Carro,Nenhum,Carro,Carro,Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-5),[Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Rio 2,[Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum])])),
                                              "Teste 4" ~: False ~=? (mapaValido (Mapa 5 [(Rio 4, [Tronco,Tronco,Tronco,Nenhum,Nenhum]),(Rio 4, [Tronco,Tronco,Nenhum,Nenhum,Tronco])])),
                                              "Teste 5" ~: False ~=? (mapaValido (Mapa 5 [(Relva, [Arvore,Arvore,Arvore,Arvore,Nenhum]),(Relva, [Nenhum,Arvore,Arvore,Arvore,Arvore])])),
                                              "Teste 6" ~: False ~=? (mapaValido (Mapa 5 [(Relva, [Arvore,Tronco,Nenhum,Nenhum,Arvore])])),
                                              "Teste 7" ~: False ~=? (mapaValido (Mapa 5 [(Relva, [Arvore,Arvore,Arvore,Nenhum,Nenhum]),(Relva, [Arvore,Arvore,Arvore,Nenhum,Nenhum]),(Relva, [Arvore,Arvore,Arvore,Nenhum,Nenhum]),(Relva, [Arvore,Arvore,Arvore,Nenhum,Nenhum]),(Relva, [Arvore,Arvore,Arvore,Nenhum,Nenhum]),(Relva, [Arvore,Arvore,Arvore,Nenhum,Nenhum])])),
                                              "Teste 8" ~: False ~=? (mapaValido (Mapa 5 [(Estrada 5, [Carro,Carro,Carro,Nenhum])]))
                                        ]


