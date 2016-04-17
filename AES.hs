import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Array

bytestring = BC.pack (padTexto "dud")

bytes = B.unpack bytestring
chars = BC.unpack bytestring

main = do
    print bytes
    print chars
    print $ elems $ carregaMatriz (take 2 bytes)
    print $ elems matriz
    print $ assocs matriz

carregaMatriz :: (Integral a, Num t, Num t1, Ix t, Ix t1) => [a] -> Array (t, t1) Integer
carregaMatriz duasLetras = 
    array ((1, 1),(2,2)) [((1,1), concatBinario (take 4 (intToBin (duasLetras !! 0)))), ((1,2), concatBinario (drop 4 (intToBin (duasLetras !! 0)))),
                          ((2,1), concatBinario (take 4 (intToBin (duasLetras !! 1)))), ((2,2), concatBinario (drop 4 (intToBin (duasLetras !! 1))))]
	
intToBin :: (Integral a, Num t) => a -> [t]
intToBin 0 = [0]
intToBin n = padBits (reverse (calculaBinario n))

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

matriz :: Array (Int, Int) Int
matriz = array ((1, 1),(2,2)) [((1,1), 0), ((1,2), 0),
                               ((2,1), 0), ((2,2), 0)]