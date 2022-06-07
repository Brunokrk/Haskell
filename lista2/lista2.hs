--Questão 1
pertence:: Eq a => a-> [a]-> Bool
pertence x [] = False
pertence x (y:ys) = (x==y) || pertence x ys

--Questão 2
intersecao :: Eq a => [a] -> [a] -> [a]
    --base 
intersecao x []    = []
intersecao [] y   = []
    --recursão
intersecao (x:xs) y = 
    if pertence x y then x : intersecao xs y else intersecao xs y

--Questão 3
concatenacao :: [a] -> [a] -> [a]
-- base
concatenacao [] ys = ys
-- recursivo
concatenacao (x : xs) ys =
  x : concatenacao xs ys

inverso :: [a] -> [a]
    --base
inverso [] = []
    --recursão
inverso (x:xs) =
    concatenacao (inverso xs) [x]

--Questão 4
nPrimeiros :: Int -> [t] ->[t]
--base
nPrimeiros n [] = []
nPrimeiros 0 _ = []
--recursao
nPrimeiros n (x:xs)=
    x : nPrimeiros (n-1) xs

nUltimos :: Int -> [t] -> [t]
--base
nUltimos n [] = []
nUltimos 0 _  = []
--recursao
nUltimos n x =
    inverso( nPrimeiros n (inverso x))

--Questão 5
soma2 :: [Int]->[Int] ->[Int]
--base 
soma2 x [] = []
soma2 [] y = []
--recursão
soma2 (x:xs) (y:ys) =
    (x + y) : soma2 xs ys

--Questao 6
criaListaN :: Int -> [Int]
criaListaN 1 = [1]
criaListaN n =
    n : criaListaN (n-1)

manipulaListaPotencia :: [Int] -> [Int] 
--base
manipulaListaPotencia [y] = [2^y]
--recursao
manipulaListaPotencia (x:xs) =
    2^x: manipulaListaPotencia xs

pot2 :: Int -> [Int]
--recursão
pot2 n =
    inverso (manipulaListaPotencia (criaListaN  n))

--Questao 7
intercalacao :: [Int]->[Int]->[Int]
--base
intercalacao [] [] = []
intercalacao [] ly = ly
intercalacao lx [] = lx
--recursão
intercalacao (x:xs) (y:ys)
    | x < y = x : intercalacao xs (y:ys)
    | x == y = x : y : intercalacao xs ys
    | otherwise = y : intercalacao (x:xs) ys


--Questao 8
menorValor :: Ord a => [a] -> a
-- base
menorValor [x] = x
-- recursivo
menorValor (x:y:xs)
  | x < y = menorValor $ x:xs
  | otherwise = menorValor $ y:xs
-- erro
menorValor [] = error "Lista nao pode ser vazia"

--Questao 9
removeElem :: Eq a => [a] -> a -> [a]
-- base
removeElem [] _ = []
-- recursivo
removeElem (x : xs) y
  | x == y = xs
  | otherwise = x: (removeElem xs y)

--Questao 10
ordenar :: Ord a => [a] -> [a]
-- base
ordenar [x] = [x]
-- recursivo
ordenar xs =
  menorValor xs : ordenar (removeElem xs (menorValor xs))

--Questao 11
insereOrd :: [Int]-> Int -> [Int]
--base
insereOrd [] x = [x]
--recursao
insereOrd xs b 
    | pertence b xs = error "Elemento já existe na lista"
    | otherwise = inserindo xs b

retornaHead :: [a]-> a
--Retorna o primeiro elemento (head) de uma lista
retornaHead [] = error "Lista vazia"
retornaHead (x:xs) = x

inserindo :: [Int]->Int->[Int]
inserindo [] x = [x]
inserindo (x:xs) y
    | y > x && y < retornaHead xs = x : y : xs
    | otherwise = x:inserindo xs y


--Questao 13
repetir :: Int->Int->[Int]
--base
repetir 0 _ = []
--recursao
repetir n e =
    e : repetir (n-1) e

--Questao 14
removeTab :: [Char]->[Char]
removeTab (x:xs) 
    | x == '\t' = ' ' : xs
    | pertence '\t' xs = x:removeTab xs

--Questão 16
inversoDupla :: [(Int, Int)] -> [(Int, Int)]
--base
inversoDupla [] = []
--recursão
inversoDupla (x :xs) = inverteElem x : inversoDupla xs

inverteElem :: (a,b) -> (b,a)
--base
inveteElem () = ()
--recursão
inverteElem (a,b) = (b,a)

--Questao 18
numString :: Int -> String
numString a = show a

--Questao 19
stringNum :: String -> Int
stringNum a = read a

--Questão 20
intParaBin :: Int -> String
-- base
intParaBin 0 = "0"
intParaBin 1 = "1"
--recursao
intParaBin n
  | mod n 2 == 0 = concatenacao (intParaBin (div n 2)) "0"
  | otherwise = concatenacao (intParaBin (div n 2)) "1"

--Questao 21
tam :: String -> Int
tam [] = 0
tam (x : xs) = 1 + tam xs

binParaInt :: String -> Int
-- base
binParaInt "0" = 0
binParaInt "1" = 1
-- recursao
binParaInt ('0' : xs) =
  binParaInt xs
binParaInt ('1' : xs) =
  2 ^ tam xs + binParaInt xs
binParaInt xs =
  error "Valor nao é binario"


