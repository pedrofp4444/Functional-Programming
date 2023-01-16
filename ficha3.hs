module Ficha3 where

-- Exercício 1

data Hora = H Int Int
            deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- Alínea a)

etapaBemConstruida :: Etapa -> Bool
etapaBemConstruida (h1,h2) = horaValida h1 && horaValida h2 && horaDepoisDe h2 h1

-- Alínea b)

viagemBemConstruida :: Viagem -> Bool
viagemBemConstruida [] = True
viagemBemConstruida ((h1,h2):t) = etapaBemConstruida (h1,h2) && viagemBemConstruida t && 
    (case t of [] -> True
               (h3,h4):t' -> h3 `horaDepoisDe` h2)

-- Alínea c)

partidaEChegada :: Viagem -> (Hora,Hora)
partidaEChegada v = (hi,hf)
    where (hi,_) = head v
          (_,hf) = last v

-- Alínea d)

tempoViagemEfetiva :: Viagem -> Hora
tempoViagemEfetiva [] = H 0 0
tempoViagemEfetiva ((h1,h2):t) = adicionaHoras (diferencaHoras h2 h1) (tempoDeViagem t)

adicionaHoras :: Hora -> Hora -> Hora
adicionaHoras (H h1 m1) (H h2 m2) = H (h1 + h2 + hExtra) (mod (m1 + mr) 60)
    where hExtra = div (m1 + m2) 60

-- Alínea e)

tempoEspera :: Viagem -> Hora
tempoEspera ((h1,h2):(h3,h4):t) = adicionaHoras (diferencaHoras h3 h2) (tempoDeEspera ((h3,h4):t))
tempoEspera _ = H 0 0

-- Alínea f)

tempoTotalViagem :: Viagem -> Hora
tempoTotalViagem v = adicionaHoras (tempoViagemEfetiva v) (tempoEspera v)

tempoTotalViagem' :: Viagem -> Hora
tempoTotalViagem' v = diferencaHoras hf hi
    where (hi,hf) = partidaEChegada v

-- Exercício 2

type Poligonal = [Ponto]

-- Alínea a)

comprimento :: Poligonal -> Double
comprimento (p1:p2:t) = dist p1 p2 + comprimento (p2:t)
comprimento _ = 0

-- Alínea b)

linhaFechada :: Poligonal -> Bool
linhaFechada p = length p >= 3 && head p == last p

-- Alínea c)

triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:ps)
    | p1 == p3 = []
    | otherwise = Triangulo p1 p2 p3 : triangula (p1:p3:ps)
triangula _ = []

-- Alínea d)

areaPol :: Poligonal -> Double
areaPol p = areaTris (triangula p)

areaTris :: [Figura] -> Double
areaTris [] = 0
areaTris (h:t) = area h + areaTris t

-- Alínea e)

mover :: Poligonal -> Ponto -> Poligonal
mover pol p = p : pol

-- Alínea f)

zoom :: Double -> Poligonal -> Poligonal
zoom z (h:t) = mover (doZoom z h t) h

doZoom :: Double -> Ponto -> Poligonal -> Poligonal
doZoom z p [] = []
doZoom z p (h:t) = Cartesiano ((x - xp) * z + xp) ((y - yp) * z + yp) : doZoom z p t
    where x = posx h
          y = posy h
          xp = posx p
          yp = posy p

-- Exercício 3

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

-- Alínea a)

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome, [email])]
acrescEmail nome email ((n,cs):t)
    | nome == n = (n, email : cs) : t
    | otherwise = (n,cs) : acrescEmail nome email t

-- Alínea b)

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nome ((n,cs):t)
    | nome == n = Just (soEmails cs)
    | otherwise = verEmails nome t

soEmails :: [Contacto] -> [String]
soEmails [] = []
soEmails (Email e : t) = e : soEmails t
soEmails (_:t) = soEmails t

-- Alínea c)

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (Email _:cs) = consTelefs cs
consTelefs (c:cs) = c : consTelefs cs

-- Alínea d)

casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa nome ((n,cs):t)
    | nome == n = numCasa cs
    | otherwise = casa nome t

numCasa :: [Contacto] -> Maybe Integer
numCasa [] = Nothing
numCasa (Casa n : t) = Just n
numCasa (_:t) = numCasa t

-- Exercício 4

type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String

data Data = D Dia Mes Ano
    deriving Show

type TabDN = [(Nome,Data)]

-- Alínea a)

procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura nome ((n,d):ts) = if nome == n then Just d else procura nome ts

-- Alínea b)

idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade (D dx mx ax) nome ((n,D d m a):ts) 
    | nome == n = Just (calculaIdade (D d m a) (D dx mx ax))
    | otherwise = idade (D dx mx ax) nome ts

calculaIdade :: Data -> Data -> Int
calculaIdade (D dn mn an) (D d m a) = if m > mn || m == mn && d > dn then a - an else a - an - 1

-- Alínea c)

anterior :: Data -> Data -> Bool
anterior (D d m a) (D d2 m2 a2) = a < a2 || (a == a2 && (m < m2 || (m == m2 && d < d2)))

-- Alínea d)

ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):ts) = insereDN (n,d) (ordena ts)

insereDN :: (Nome,Data) -> TabDN -> TabDN
insereDN (n,d) [] = [(n,d)]
insereDN (n,d) ((nh,dh):t) | anterior d dh = (n,d) : (nh,dh) : t
                           | otherwise = (nh,dh) : insereDN (n,d) t

-- Alínea e)

porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade data tabela = porIdadeAux data (ordena tabela)

porIdadeAux :: Data -> TabDN -> [(Nome,Int)]
porIdadeAux _ [] = []
porIdadeAux d ((nh,dh):t) = porIdadeAux d t ++ [(nh, calculaIdade dh d)]

-- Exercício 5

data Movimento = Credito Float | Debito Float
    deriving Show

data Data = D Int Int Int
    deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
    deriving Show

-- Alínea a)

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext si ((_,_,mov):t)) valor = if getValor mov > valor then mov : extValor (Ext si t) valor else extValor (Ext si t) valor

getValor :: Movimento -> Float
getValor (Credito x) = x
getValor (Debito x) = x

-- Alínea b)

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext si ((data,desc,mov):t)) listaStr = if desc `elem` listaStr then (data,mov) : filtro (Ext si t) listaStr else filtro (Ext si t) listaStr

-- Alínea c)

creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext si ((_,_,Credito x):t)) = (x + cr, dr)
    where (cr,dr) = creDeb (Ext si t)
creDeb (Ext si ((_,_,Debito x):t)) = (cr, x + dr)
    where (cr,dr) = creDeb (Ext si t)

creDeb' :: Extracto -> (Float,Float)
creDeb' (Ext _ []) = (0,0)
creDeb' (Ext si ((_,_,mov):t)) = (c + cr, d + dr)
    where (cr,dr) = creDeb (Ext si t)
          (c,d) = case mov of Credito x -> (x,0)
                              Debito x -> (0,x)

-- Alínea d)

saldo :: Extracto -> Float
saldo (Ext si []) = si
saldo (Ext si ((_,_,Debito x):t)) = saldo (Ext (si + x) t)
saldo (Ext si ((_,_,Credito x):t)) = saldo (Ext (si - x) t)