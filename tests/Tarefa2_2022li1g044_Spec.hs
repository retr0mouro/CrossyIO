module Tarefa2_2022li1g044_Spec where

import LI12223
import Tarefa2_2022li1g044
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test ["Teste 1" ~: Mapa 4 [(Rio (-2),[Nenhum,Tronco,Tronco,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Nenhum])] ~=? estendeMapa (Mapa 4 [(Relva,[Arvore,Arvore,Nenhum,Nenhum]),(Rio 2,[Tronco,Nenhum,Tronco,Tronco])]) 66,
                                              "Teste 2" ~: Mapa 4 [(Relva,[Arvore,Arvore,Nenhum,Arvore]),(Relva,[Arvore,Nenhum,Arvore,Arvore]),(Rio 3,[Tronco,Nenhum,Tronco,Tronco])] ~=? estendeMapa (Mapa 4 [(Relva,[Arvore,Nenhum,Arvore,Arvore]),(Rio 3,[Tronco,Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Arvore,Arvore])]) 17,  
                                              "Teste 3" ~: Mapa 4 [(Estrada 2,[Nenhum,Carro,Nenhum,Carro]),(Relva,[Arvore,Arvore,Nenhum,Nenhum]),(Estrada 3,[Carro,Carro,Carro,Nenhum])] ~=? estendeMapa (Mapa 4 [(Relva,[Arvore,Arvore,Nenhum,Nenhum]),(Estrada 3 ,[Carro,Carro,Carro,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Arvore])]) 100,
                                              "Teste 4" ~: Mapa 5 [(Rio (-2),[Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Estrada 3,[Carro,Carro,Carro,Nenhum,Nenhum])] ~=? estendeMapa (Mapa 5 [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Estrada 3 ,[Carro,Carro,Carro,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Arvore,Arvore])]) 41,
                                              "Teste 5" ~: Mapa 12 [(Relva,[Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore]),(Estrada 3,[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Carro,Nenhum,Carro])] ~=? estendeMapa (Mapa 12 [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore]),(Estrada 3 ,[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Carro,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum])]) 0
                                            ]
