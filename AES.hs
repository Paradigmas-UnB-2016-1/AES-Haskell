import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Array

--arquivoEntrada <- openFile "texto.txt" ReadMode

bytestring = BC.pack (padTexto "dud")

bytes = B.unpack bytestring
chars = BC.unpack bytestring

main = do
    print bytes
    print chars
    print $ elems $ carregaMatriz (take 2 bytes)
    print (cifraTexto bytes)

cifraTexto [] = ""
--cifraTexto bytes =
--return (matrizToString (cifraMatriz (carregaMatriz (take 2 bytes))) ++ cifraTexto (drop 2 bytes))

matrizToString matriz =
    concatBinario $ matriz!(1,1):matriz!(1,2):matriz!(2,1):matriz!(2,2):[]

--cifraMatrix matriz = addRoundKey(shiftRows(subtituteNibbles(addRoundKey(mixColumns(shiftRows(subtituteNibbles(addRoundKey(matriz))))))))

--addRoundKey matriz =

--subtituteNibbles matriz =

shiftRows matriz =
    (array ((1, 1),(2,2))  [((1,1), matriz!(1,1)), ((1,2), matriz!(1,2)),
                            ((2,1), matriz!(2,2)), ((2,2), matriz!(2,1))])

mixColumns matriz =
    array ((1, 1),(2,2)) [((1,1), multiplicaPolinomio (matriz!(1,1):matriz!(2,1):[])!(1,1)), 
                          ((1,2), multiplicaPolinomio (matriz!(1,2):matriz!(2,2):[])!(1,1)),
                          ((2,1), multiplicaPolinomio (matriz!(1,1):matriz!(2,1):[])!(2,1)),
                          ((2,2), multiplicaPolinomio (matriz!(1,2):matriz!(2,2):[])!(2,1))]

multiplicaPolinomio coluna = 
    array ((1, 1),(2,1)) [((1,1), polinomio!(1,1)*coluna!!0 + polinomio!(1,2)*coluna!!1), 
                          ((2,1), polinomio!(2,1)*coluna!!0 + polinomio!(2,2)*coluna!!1)]

carregaMatriz :: (Integral a, Num t, Num t1, Ix t, Ix t1) => [a] -> Array (t, t1) Integer
carregaMatriz duasLetras = 
    array ((1, 1),(2,2)) [((1,1), binArrayToBin duasLetras 1 1), 
                          ((1,2), binArrayToBin duasLetras 1 2),
                          ((2,1), binArrayToBin duasLetras 2 1),
                          ((2,2), binArrayToBin duasLetras 2 2)]

binArrayToBin :: (Eq a, Integral a1, Num a) => [a1] -> Int -> a -> Integer
binArrayToBin duasLetras linha coluna
    | coluna == 1 = concatBinario (take 4 (intToBinArray (duasLetras !! (linha - 1))))
    | coluna == 2 = concatBinario (drop 4 (intToBinArray (duasLetras !! (linha - 1))))

intToBinArray :: (Integral a, Num t) => a -> [t]
intToBinArray 0 = [0]
intToBinArray n = padBits (reverse (calculaBinario n))

calculaBinario :: (Integral a, Num t) => a -> [t]
calculaBinario 0 = []
calculaBinario n | n `mod` 2 == 1 = 1 : calculaBinario (n `div` 2)
                 | n `mod` 2 == 0 = 0 : calculaBinario (n `div` 2)

padBits :: Num a => [a] -> [a]
padBits xs = replicate (8 - length ys) 0 ++ ys
    where ys = take 8 xs

padTexto :: [Char] -> [Char]
padTexto texto = texto ++ replicate ((length texto) `mod` 2) ' '

concatBinario :: [Integer] -> Integer
concatBinario = read . concatMap show
