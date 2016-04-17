import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Array
import Data.Bits
import Data.Char (digitToInt)

--arquivoEntrada <- openFile "texto.txt" ReadMode

bytestring = BC.pack (padTexto "dud")

bytes = B.unpack bytestring
chars = BC.unpack bytestring

main = do
    print bytes
    print chars
    print $ elems $ carregaMatriz (take 2 bytes)
--  print (cifraTexto bytes)

-- cifraTexto [] = ""
--cifraTexto bytes =
--return (matrizToString (cifraMatriz (carregaMatriz (take 2 bytes))) ++ cifraTexto (drop 2 bytes))

matrizToString matriz =
    concatBinario $ matriz!(1,1):matriz!(1,2):matriz!(2,1):matriz!(2,2):[]

--cifraMatrix matriz = addRoundKey(shiftRows(subtituteNibbles(addRoundKey(mixColumns(shiftRows(subtituteNibbles(addRoundKey(matriz))))))))

--addRoundKey matriz =

operationXOR x y | x == 1 && y == 0 = 0
        | x == 0 && y == 1 = 0
        | otherwise = 1

subtituteNibbles matriz =
    array ((1, 1),(2,2)) [((1,1), multiplicaPolinomio4por4 (pad4Bits (toDigits(matriz!(1,1))))),
                          ((1,2), multiplicaPolinomio4por4 (pad4Bits (toDigits(matriz!(1,2))))),
                          ((2,1), multiplicaPolinomio4por4 (pad4Bits (toDigits(matriz!(2,1))))),
                          ((2,2), multiplicaPolinomio4por4 (pad4Bits (toDigits(matriz!(2,2)))))]

pad4Bits :: Num a => [a] -> [a]
pad4Bits xs = replicate (4 - length ys) 0 ++ ys
    where ys = take 4 xs

toDigits :: Integer -> [Integer]
toDigits = map (fromIntegral . digitToInt) . show

multiplicaPolinomio4por4 bits = 
    --concatBinario(
    polinomio4por4!(1,1)*bits!!0 + polinomio4por4!(1,2)*bits!!1 + polinomio4por4!(1,3)*bits!!2 + polinomio4por4!(1,4)*bits!!3 :
    polinomio4por4!(2,1)*bits!!0 + polinomio4por4!(2,2)*bits!!1 + polinomio4por4!(2,3)*bits!!2 + polinomio4por4!(2,4)*bits!!3 :
    polinomio4por4!(3,1)*bits!!0 + polinomio4por4!(3,2)*bits!!1 + polinomio4por4!(3,3)*bits!!2 + polinomio4por4!(3,4)*bits!!3 :
    polinomio4por4!(4,1)*bits!!0 + polinomio4por4!(4,2)*bits!!1 + polinomio4por4!(4,3)*bits!!2 + polinomio4por4!(4,4)*bits!!3 :
    []
    --)

shiftRows matriz =
    array ((1, 1),(2,2))  [((1,1), matriz!(1,1)), ((1,2), matriz!(1,2)),
                            ((2,1), matriz!(2,2)), ((2,2), matriz!(2,1))]

mixColumns matriz =
    array ((1, 1),(2,2)) [((1,1), multiplicaPolinomio2por2 (matriz!(1,1):matriz!(2,1):[])!(1,1)), 
                          ((1,2), multiplicaPolinomio2por2 (matriz!(1,2):matriz!(2,2):[])!(1,1)),
                          ((2,1), multiplicaPolinomio2por2 (matriz!(1,1):matriz!(2,1):[])!(2,1)),
                          ((2,2), multiplicaPolinomio2por2 (matriz!(1,2):matriz!(2,2):[])!(2,1))]

multiplicaPolinomio2por2 coluna = 
    array ((1, 1),(2,1)) [((1,1), polinomio2por2!(1,1)*coluna!!0 + polinomio2por2!(1,2)*coluna!!1), 
                          ((2,1), polinomio2por2!(2,1)*coluna!!0 + polinomio2por2!(2,2)*coluna!!1)]

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
intToBinArray n = pad8Bits (reverse (calculaBinario n))

calculaBinario :: (Integral a, Num t) => a -> [t]
calculaBinario 0 = []
calculaBinario n | n `mod` 2 == 1 = 1 : calculaBinario (n `div` 2)
                 | n `mod` 2 == 0 = 0 : calculaBinario (n `div` 2)

pad8Bits :: Num a => [a] -> [a]
pad8Bits xs = replicate (8 - length ys) 0 ++ ys
    where ys = take 8 xs

padTexto :: [Char] -> [Char]
padTexto texto = texto ++ replicate ((length texto) `mod` 2) ' '

concatBinario :: [Integer] -> Integer
concatBinario = read . concatMap show

teste =
    subtituteNibbles meuArray

polinomio2por2 = 
    array ((1, 1),(2,2)) [((1,1), 1), 
                          ((1,2), 4),
                          ((2,1), 4),
                          ((2,2), 1)]

polinomio4por4 = 
    array ((1, 1),(4,4)) [((1,1), 1), ((1,2), 0), ((1,3), 1), ((1,4), 1),
                          ((2,1), 1), ((2,2), 1), ((2,3), 0), ((2,4), 1), 
                          ((3,1), 1), ((3,2), 1), ((3,3), 1), ((3,4), 0),
                          ((4,1), 0), ((4,2), 1), ((4,3), 1), ((4,4), 1)]

meuArray = 
    array ((1, 1),(2,2)) [((1,1), 1), 
                          ((1,2), 2),
                          ((2,1), 3),
                          ((2,2), 4)]

