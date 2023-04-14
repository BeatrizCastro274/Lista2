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

data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo deriving (Show, Enum, Eq)

tercas :: [Dia] -> [Dia]
tercas dias = filter (== Terca) dias