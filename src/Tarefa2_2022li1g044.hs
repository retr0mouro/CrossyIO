module Tarefa2_2022li1g044 where


import LI12223
import Data.List


-- | = Função principal
-- | estendeMapa : Função usa a 
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa n t) i = Mapa n (estTerreno (Mapa n t) i:init t)


-- | A função auxiliar estTerreno cria uma nova linha com proximosTerrenosValidos e, usando a função estObst, criar obstáculos
estTerreno :: Mapa -> Int -> (Terreno,[Obstaculo])
estTerreno (Mapa n t) i 
    | i<=33 = (head (proximosTerrenosValidos (Mapa n t)) , estObst (head (proximosTerrenosValidos (Mapa n t)),[]) n i)
    | i <=80 && i>33 = (last (proximosTerrenosValidos (Mapa n t)) , estObst (last (proximosTerrenosValidos (Mapa n t)),[]) n i)
    | otherwise = ((!!) (proximosTerrenosValidos (Mapa n t)) 1 , estObst ((!!) (proximosTerrenosValidos (Mapa n t)) 1,[]) n i)

-- | A função estObst usa o novo terreno gerado e a função proximosObstaculosValidos para criar obstáculos adequados 
estObst :: (Terreno, [Obstaculo]) -> Int -> Int -> [Obstaculo]
estObst (Relva, o) n i 
    | i<=50 && length o <= n = head (proximosObstaculosValidos n (Relva, o)): estObst (Relva, o++[head (proximosObstaculosValidos n (Relva, o))]) (n-1) (mod (i-7) 100)
    | i>50 && length o <= n = last (proximosObstaculosValidos n (Relva, o)): estObst (Relva, o++[last (proximosObstaculosValidos n (Relva, o))]) (n-1) (mod (i-7) 100)
    | otherwise = take n o

estObst (Rio _, o) n i 
    | i<=50 && length o <= n = head (proximosObstaculosValidos n (Rio 0, o)): estObst (Rio 0, o++[head (proximosObstaculosValidos n (Rio 0, o))]) (n-1) (mod (i-7) 100)
    | i>50 && length o <= n = last (proximosObstaculosValidos n (Rio 0, o)): estObst (Rio 0, o++[last (proximosObstaculosValidos n (Rio 0, o))]) (n-1) (mod (i-7) 100)
    | otherwise = take n o

estObst (Estrada _, o) n i 
    | i<=50 && length o <= n = head (proximosObstaculosValidos n (Estrada 0, o)): estObst (Estrada 0, o++[head (proximosObstaculosValidos n (Estrada 0, o))]) (n-1) (mod (i-7) 100)
    | i>50 && length o <= n = last (proximosObstaculosValidos n (Estrada 0, o)): estObst (Estrada 0, o++[last (proximosObstaculosValidos n (Estrada 0, o))]) (n-1) (mod (i-7) 100)
    | otherwise = take n o


-- | A função proximosTerrenosValidos dá os possíveis terrenos a colocar na nova linha criada de acorda com os terrenos das linhas anteriores
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa n []) = [Relva, Relva]
proximosTerrenosValidos (Mapa n ((e ,o):t)) 
    | isPrefixOf [Relva,Relva,Relva,Relva,Relva] (abranda ((e ,o):t)) = [Rio 2, Estrada 1, Estrada (-2)]
    | e == Relva = [Relva, Estrada 3, Rio 3]
    | isPrefixOf [Rio 0,Rio 0,Rio 0,Rio 0] (abranda ((e ,o):t)) = [Estrada 2, Estrada(-1), Relva] 
    | e == Rio 1 = [Rio (-2), Estrada (-2), Relva]
    | e == Rio 2 = [Rio (-1), Estrada (-1), Relva]
    | e == Rio (-1) = [Rio 2, Estrada 2, Relva]
    | e == Rio (-2) = [Rio 1, Estrada 1, Relva]
    | isPrefixOf [Estrada 0,Estrada 0,Estrada 0,Estrada 0,Estrada 0] (abranda ((e ,o):t)) = [Rio (-1), Rio 1, Relva]
    | e == Estrada 1 = [Estrada (-1), Estrada 2, Relva]
    | e == Estrada 2 = [Estrada 1, Estrada (-2), Relva]
    | e == Estrada (-1) = [Rio (-2), Estrada 2, Relva]
    | e == Estrada (-2) = [Rio (-1), Estrada 1, Relva]
    | otherwise = [Rio 1, Estrada 1, Relva]         
   
-- | A função proximosObstaculosValidos dá os possíveis obstáculos a colocar na nova linha criada dependendo do tipo de terreno
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos n (Relva, o) = proxArvVal n (Relva, o)
proximosObstaculosValidos n (Rio _, o) = proxTroncoVal n (Rio 0, o)
proximosObstaculosValidos n (Estrada _, o) = proxCarroVal n (Estrada 0, o)

-- | Função usada para colocar os terrenos todos com a mesma velocidade
abranda :: [(Terreno, [Obstaculo])] -> [Terreno]
abranda [] = []
abranda ((Relva ,o):t) = Relva:abranda t
abranda ((Rio _,o):t) = Rio 0:abranda t
abranda ((Estrada _,o):t) = Estrada 0:abranda t

-- | A função proxArvVal dá possíveis obstáculos a colocar na nova linha criada de acordo com os obstáculos anteriores, quando o terreno é Relva
proxArvVal :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proxArvVal n (Relva, o) 
    | notElem Nenhum o = [Nenhum]
    | otherwise = [Nenhum, Arvore]


-- | A função proxTroncoVal é análoga à função proxArvVal, sendo aplicada quando o terreno é Rio
proxTroncoVal :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proxTroncoVal n (Rio _, o) 
    | notElem Tronco o  = [Tronco]
    | notElem Nenhum o  = [Nenhum]
    | verfList [Tronco,Tronco,Tronco,Tronco,Tronco] o = [Nenhum]
    | otherwise = [Nenhum, Tronco]


-- | A função proxCarroVal é análoga à função proxArvVal, sendo aplicada quando o terreno é Estrada
proxCarroVal :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proxCarroVal n (Estrada _, o)    
    | notElem Carro o  = [Carro]
    | notElem Nenhum o  = [Nenhum]
    | verfList [Carro,Carro,Carro] o= [Nenhum]
    | otherwise = [Nenhum, Carro]


-- | A função verfList analisa se uma lista está dentro de outra lista ou não
verfList :: Eq a => [a] -> [a] -> Bool
verfList [] _ = True
verfList _ [] = False
verfList t o 
    | t== head (group o) = True
    | otherwise = verfList t (concat (tail (group o)))