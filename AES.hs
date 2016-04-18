import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Array
import Data.Bits
import Data.Char (digitToInt)

--arquivoEntrada <- openFile "texto.txt" ReadMode

byteStringTexto = BC.pack (padTexto "dud")

byteStringChave = BC.pack "6c8d"

bytesTexto = B.unpack byteStringTexto
charsTexto = BC.unpack byteStringTexto

bytesChave = B.unpack byteStringTexto

main = do
    print bytesTexto
    print charsTexto
--    cifraTexto bytesTexto
    
--  print (cifraTexto bytes)

--cifraTexto [] = ""
--cifraTexto bytesTexto =
--    return (matrizToString cifraMatriz $ (carregaMatriz (take 2 bytesTexto)) (carregaMatriz (bytesChave)) ++ cifraTexto (drop 2 bytesTexto))

--matrizToString matriz =
--    concatBinario $ matriz!(1,1):matriz!(1,2):matriz!(2,1):matriz!(2,2):[]

--cifraMatrix :: Array (t2, t3) Integer -> Array (t, t1) Integer -> Array (t, t1) Integer 
--cifraMatrix matriz chave1 = 
--	let chave2 = expandir chave1 1
--	    chave3 = expandir chave2 2
--	in addRoundKey $ (chave3) (shiftRows $ substituteNibbles $ addRoundKey $ (chave2) (mixColumns $ shiftRows $ substituteNibbles $ addRoundKey $ chave1 matriz))

addRoundKey chave matriz = 
    array ((1, 1),(2,2)) [((1,1), realizaXorBitPorBit (binToBinArray8Bits $ chave!(1,1)) (binToBinArray8Bits $ matriz!(1,1))),
                          ((1,2), realizaXorBitPorBit (binToBinArray8Bits $ chave!(1,2)) (binToBinArray8Bits $ matriz!(1,2))),
                          ((2,1), realizaXorBitPorBit (binToBinArray8Bits $ chave!(2,1)) (binToBinArray8Bits $ matriz!(2,1))),
                          ((2,2), realizaXorBitPorBit (binToBinArray8Bits $ chave!(2,2)) (binToBinArray8Bits $ matriz!(2,2)))]

--Função necessita receber duas listas com 8 elementos inteiros
realizaXorBitPorBit bitsWa bitsWb' =
    concatBinario $ (bitsWa)!!0 `xor` (bitsWb')!!0 : 
                    (bitsWa)!!1 `xor` (bitsWb')!!1 : 
                    (bitsWa)!!2 `xor` (bitsWb')!!2 : 
                    (bitsWa)!!3 `xor` (bitsWb')!!3 :
                    (bitsWa)!!4 `xor` (bitsWb')!!4 : 
                    (bitsWa)!!5 `xor` (bitsWb')!!5 : 
                    (bitsWa)!!6 `xor` (bitsWb')!!6 : 
                    (bitsWa)!!7 `xor` (bitsWb')!!7 : []

substituteNibbles matriz =
    array ((1, 1),(2,2)) [((1,1), substituiBitsSbox $ matriz!(1,1)),
                          ((1,2), substituiBitsSbox $ matriz!(1,2)),
                          ((2,1), substituiBitsSbox $ matriz!(2,1)),
                          ((2,2), substituiBitsSbox $ matriz!(2,2))]

substituiBitsSbox :: Integer -> Integer
substituiBitsSbox bits = sBox!(binArrayToInt(take 2 $ binToBinArray(bits)) + 1, (binArrayToInt(drop 2 $ binToBinArray(bits))+1)) 

binArrayToInt::[Integer] -> Integer 
binArrayToInt bits = bits!!0 * 2 + bits!!1 * 1

--finalXorSubstituteNibbles $ multiplicaPolinomio4por4 (binToBinArray(matriz!(1,1)))
--finalXorSubstituteNibbles bits =
--	bits!!0 `xor` 1 :    
--	bits!!1 `xor` 0 :    
--	bits!!2 `xor` 0 :    
--	bits!!3 `xor` 1 :    []

pad4Bits :: Num a => [a] -> [a]
pad4Bits xs = replicate (4 - length ys) 0 ++ ys
    where ys = take 4 xs

pad8Bits :: Num a => [a] -> [a]
pad8Bits xs = replicate (8 - length ys) 0 ++ ys
    where ys = take 8 xs

binToBinArray :: Integer -> [Integer]
binToBinArray = pad4Bits . map (fromIntegral . digitToInt) . show

binToBinArray8Bits :: Integer -> [Integer]
binToBinArray8Bits = pad8Bits . map (fromIntegral . digitToInt) . show

--multiplicaPolinomio4por4 :: [Integer] -> [Integer]
--multiplicaPolinomio4por4 bits = 
--    --concatBinario(
--    polinomio4por4!(1,1)*bits!!0 `xor` polinomio4por4!(1,2)*bits!!1 `xor` polinomio4por4!(1,3)*bits!!2 `xor` polinomio4por4!(1,4)*bits!!3 :
--    polinomio4por4!(2,1)*bits!!0 `xor` polinomio4por4!(2,2)*bits!!1 `xor` polinomio4por4!(2,3)*bits!!2 `xor` polinomio4por4!(2,4)*bits!!3 :
--    polinomio4por4!(3,1)*bits!!0 `xor` polinomio4por4!(3,2)*bits!!1 `xor` polinomio4por4!(3,3)*bits!!2 `xor` polinomio4por4!(3,4)*bits!!3 :
--    polinomio4por4!(4,1)*bits!!0 `xor` polinomio4por4!(4,2)*bits!!1 `xor` polinomio4por4!(4,3)*bits!!2 `xor` polinomio4por4!(4,4)*bits!!3 :
--    []
--    --)


shiftRows :: (Num t, Num t1, Num t2, Num t3, Ix t, Ix t1, Ix t2, Ix t3) => Array (t2, t3) e -> Array (t, t1) e
shiftRows matriz =
    array ((1, 1),(2,2))  [((1,1), matriz!(1,1)), ((1,2), matriz!(1,2)),
                           ((2,1), matriz!(2,2)), ((2,2), matriz!(2,1))]

mixColumns :: (Num t, Num t1, Num t2, Num t3, Ix t, Ix t1, Ix t2, Ix t3) => Array (t2, t3) Integer -> Array (t, t1) Integer
mixColumns matriz =
    array ((1, 1),(2,2)) [((1,1), multiplicaPolinomio2por2 (matriz!(1,1):matriz!(2,1):[])!(1,1)), 
                          ((1,2), multiplicaPolinomio2por2 (matriz!(1,2):matriz!(2,2):[])!(1,1)),
                          ((2,1), multiplicaPolinomio2por2 (matriz!(1,1):matriz!(2,1):[])!(2,1)),
                          ((2,2), multiplicaPolinomio2por2 (matriz!(1,2):matriz!(2,2):[])!(2,1))]

multiplicaPolinomio2por2 :: (Num t, Num t1, Ix t, Ix t1) => [Integer] -> Array (t, t1) Integer
multiplicaPolinomio2por2 coluna = 
    array ((1, 1),(2,1)) [((1,1), polinomio2por2!(1,1)*coluna!!0 + polinomio2por2!(1,2)*coluna!!1), 
                          ((2,1), polinomio2por2!(2,1)*coluna!!0 + polinomio2por2!(2,2)*coluna!!1)]

carregaMatriz :: (Integral a1, Num t, Num t1, Ix t, Ix t1) => [a1] -> Array (t, t1) Integer
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

padTexto :: [Char] -> [Char]
padTexto texto = texto ++ replicate ((length texto) `mod` 2) ' '

concatBinario :: [Integer] -> Integer
concatBinario = read . concatMap show

polinomio2por2 = 
    array ((1, 1),(2,2)) [((1,1), 1), 
                          ((1,2), 4),
                          ((2,1), 4),
                          ((2,2), 1)]

--polinomio4por4 = 
--    array ((1, 1),(4,4)) [((1,1), 1), ((1,2), 0), ((1,3), 1), ((1,4), 1),
--                          ((2,1), 1), ((2,2), 1), ((2,3), 0), ((2,4), 1), 
--                          ((3,1), 1), ((3,2), 1), ((3,3), 1), ((3,4), 0),
--                          ((4,1), 0), ((4,2), 1), ((4,3), 1), ((4,4), 1)]

sBox = array ((1,1),(4,4)) [((1,1), 1001), ((1,2), 0100), ((1,3), 1010), ((1,4), 1011),
                            ((2,1), 1101), ((2,2), 0001), ((2,3), 1000), ((2,4), 0101),
                            ((3,1), 0110), ((3,2), 0010), ((3,3), 0000), ((3,4), 0011),
                            ((4,1), 1100), ((4,2), 1110), ((4,3), 1111), ((4,4), 0111)]

--sBoxInversa = 
--	array ((1,1),(4,4)) [ ((1,1), ), ((1,2), ), ((1,3), ), ((1,4), ),
--						   ((2,1), ), ((2,2), ), ((2,3), ), ((2,4), ),
--						   ((3,1), ), ((3,2), ), ((3,3), ), ((3,4), ),
--						   ((4,1), ), ((4,2), ), ((4,3), ), ((4,4), )]

teste = substituteNibbles meuArray

chave = array ((1, 1),(2,2)) [((1,1), 0010), 
                              ((1,2), 1101),
                              ((2,1), 0101),
                              ((2,2), 0101)]

meuArray =  array ((1, 1),(2,2)) [((1,1), 1), 
                                  ((1,2), 11),
                                  ((2,1), 100),
                                  ((2,2), 101)]

