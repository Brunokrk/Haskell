import System.IO
import GHC.Unicode
import Data.List 
import Data.List 

--Aluno Bruno Marchi Pires
--O Arquivo txt foi gerado automaticamente por um gerador na web

type Doc = String
type Linha = String
type Palavra = String

--b)
numLinhas :: [Linha] -> [(Int, Linha)]
numLinhas [] = []
numLinhas x= contadorL [1..length x] x

contadorL :: [Int] -> [Linha] -> [(Int, Linha)]
contadorL [] [] = []
contadorL (x:xs) (y:ys) = (x,y) : contadorL xs ys

--c)
sanitize :: [(Int, Linha)] -> [(Int, Linha)]
sanitize []=[]
sanitize (x:xs) = (fst x, removeEspeciais (snd x)) : sanitize xs

removeEspeciais :: String -> String
removeEspeciais [] = []
removeEspeciais (x:xs)
    | x ==' ' = x : removeEspeciais xs 
    | not (isAlpha x) = removeEspeciais xs
    | otherwise = x : removeEspeciais xs

numeraPalavras :: [(Int, Linha)] -> [(Int, Palavra)]
numeraPalavras [] = []
numeraPalavras (x:xs) = contadorP (fst(x), words(snd (x))) ++ (numeraPalavras xs) 

contadorP :: (Int, [String]) -> [(Int, String)]
contadorP (_,[]) = []
contadorP (x, (y:ys)) = (x, y) : contadorP (x, ys)

removePorTamanho :: [(Int, Palavra)] -> [(Int, Palavra)]
removePorTamanho [] = []
removePorTamanho (x:xs) 
    | length (snd x) < 3 = removePorTamanho xs
    | otherwise = x: removePorTamanho xs


--d)
ordenar :: [(Int, Palavra)] -> [(Int, Palavra)]
ordenar [] = []
ordenar lst = sortBy compara lst

compara :: (Ord a, Ord b) => (a, b) -> (a,b) -> Ordering
compara (a1,b1) (a2,b2)
    | b1 > b2 = GT --greater than
    | b1 == b2 = EQ --equal
    | otherwise = LT --less than

--e)
agrupar :: [(Int, Palavra)] -> [([Int], Palavra)]
agrupar [] = []
agrupar (x:xs) = ( descobreLinhas (snd x) (x:xs), snd x ) : agrupar xs

descobreLinhas:: String -> [(Int, String)] -> [Int]
descobreLinhas _ [] = []
descobreLinhas word (x:xs)
    | word /= snd x = descobreLinhas word xs
    | otherwise = fst x : descobreLinhas word xs


--f)
eliminarRep :: [([Int], Palavra)] -> [([Int], Palavra)]
eliminarRep [] = []
eliminarRep (x:xs) = ( removeDups (fst x), snd x) : eliminarRep xs

removeDups :: (Eq a) => [a] -> [a]
removeDups [] =  []
removeDups (xs : []) = [xs]
removeDups (x1:x2:xs)
    | x1 == x2     =      removeDups (x2 : xs)
    | otherwise    = x1 : removeDups (x2 : xs)


construirIndice :: Doc -> [([Int], Palavra)]
construirIndice doc =
    let linhas = lines doc in
    let enumeradas = numLinhas linhas in
    let semSinais = sanitize enumeradas in
    let palavras = numeraPalavras semSinais in
    let palavrasAprovadas = removePorTamanho palavras in
    let ordenadas = ordenar palavrasAprovadas in
    let agrupadas = agrupar ordenadas in
    eliminarRep agrupadas

main :: IO ()
main = do
    -- Abre um arquivo!
    h <- openFile "entrada.txt" ReadMode
    -- Pega o conteúdo do arquivo!
    conteudo <- hGetContents h
    -- Imprime o resultado!
    print (construirIndice conteudo)
    
    
    -- Fecha o arquivo!
    hClose h

