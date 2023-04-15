-- Lista de Exercícios
-- Beatriz Castro, RA 0050831811004 / Caio Oliveira, RA 0050831911038

-- Exercício 4.1
media :: [Double] -> Double
media xs = sum xs / fromIntegral (length xs)

-- Exercício 4.2

palindromos :: [String] -> [String]
palindromos = filter (\x -> x == reverse x)

-- Exercício 4.4
isPrimo :: Int -> Bool
isPrimo n = n > 1 && all (\x -> n `mod` x /= 0) [2 .. limit]
  where
    limit = floor . sqrt $ fromIntegral n

primeList :: [Int] -> [Int]
primeList = filter isPrimo

-- Exercício 4.7

data DiaSemana = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo deriving (Show, Enum, Eq)

tercas :: [Dia] -> [Dia]
tercas dia = filter (== Terca) dias

-- Exercício 4.8

-- 4.8)
data Correncia = Real | Dolar deriving (Show, Eq)

data Dinheiro = Dinheiro {valor :: Double, correncia :: Correncia} | Null deriving (Show)

converteDinheiro :: Dinheiro -> Dinheiro
converteDinheiro (Dinheiro valor Real) = Dinheiro (valor * 5.20) Dolar
converteDinheiro (Dinheiro valor Dolar) = Dinheiro (valor / 5.20) Real

converteDinheiroComGuard :: Dinheiro -> Dinheiro
converteDinheiroComGuard (Dinheiro valor correncia)
  | correncia == Real = Dinheiro (valor * 5.20) Dolar
  | correncia == Dolar = Dinheiro (valor / 5.20) Real

converteListaDeDinheiros :: [Dinheiro] -> [Dinheiro]
converteListaDeDinheiros = map converteDinheiro

-- pattern matching
converteParaDolares :: Dinheiro -> Dinheiro
converteParaDolares (Dinheiro valor Real) = Dinheiro (valor * 5.20) Dolar
converteParaDolares (Dinheiro valor Dolar) = Dinheiro valor Dolar
converteParaDolares _ = Null -- otherwise

converteParaDolaresComGuard :: Dinheiro -> Dinheiro
converteParaDolaresComGuard (Dinheiro valor correncia)
  | correncia == Real = Dinheiro (valor * 5.20) Dolar
  | correncia == Dolar = Dinheiro valor correncia
  | otherwise = Null

converteListaDeDinheirosParaDolares :: [Dinheiro] -> [Dinheiro]
converteListaDeDinheirosParaDolares = map converteParaDolares

converteParaReaisComGuard :: Dinheiro -> Dinheiro
converteParaReaisComGuard (Dinheiro valor correncia)
  | correncia == Real = Dinheiro valor Real
  | correncia == Dolar = Dinheiro (valor / 5.20) Real
  | otherwise = Null

converteListaDeDinheirosParaReais :: [Dinheiro] -> [Dinheiro]
converteListaDeDinheirosParaReais = map converteParaReaisComGuard

-- 4.8) a)

verificaDolares :: Dinheiro -> Bool
verificaDolares (Dinheiro valor correncia)
  | correncia == Dolar = True
  | otherwise = False
verificaDolares _ = False

filtraDolares :: [Dinheiro] -> [Dinheiro]
filtraDolares = filter verificaDolares

-- 4.8) b)

somarDolares :: [Dinheiro] -> Double
somarDolares xs = foldl (\acc (Dinheiro valor correncia) -> acc + valor) 0 $ filtraDolares xs

-- 4.8) c)
contarDolares :: [Dinheiro] -> Int
contarDolares xs = foldl (\acc _ -> acc + 1) 0 $ filtraDolares xs

contarDolaresSimp :: [Dinheiro] -> Int
contarDolaresSimp = length . filtraDolares

-- Exercicio 4.9 a)
contaNegativos :: [Int] -> Int
contaNegativos xs = foldl (\acc x -> if x < 0 then acc + 1 else acc) 0 xs

-- 4.9 b)
countP :: String -> Int
countP str = foldl (\acc char -> if char == 'P' then acc + 1 else acc) 0 str

-- 4.9 c) (DiaSemanas já declarado em exercício anterior)

contarSabados :: [DiaSemana] -> Int
contarSabados = foldl (\acc dia -> if dia == Sabado then acc + 1 else acc) 0

diaToInt :: DiaSemana -> Int
diaToInt dia = fromEnum dia + 1

somaDiasSemana :: [DiaSemana] -> Int
somaDiasSemana dias = foldl (\acc dia -> acc + diaToInt dia) 0 dias