import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.IO
import Control.Monad
import Data.List
import Data.Array
import Data.Bits
import Data.Char (digitToInt)
import Data.Char (intToDigit)
import Numeric (showIntAtBase)

main :: IO ()
main = do
    putStrLn "\n\n**************************************"
    putStrLn "***********   Menu S-AES   ***********"
    putStrLn "**************************************"
    putStrLn "1) Cifrar Texto"
    putStrLn "2) Decifrar Texto"
    putStrLn "9) Sair"
    putStr "Digite a opção desejada:  "
    opcao <- getLine
    putStr "Digite o texto:    "
    texto <- getLine
    putStr "Digite a chave:    "
    chave <- getLine
    menu opcao texto chave

menu opcao texto chave
    | opcao == "1" = putStrLn ("\nTexto cifrado: " ++ cifraTexto texto chave)
    | opcao == "2" = putStrLn ("\nTexto decifrado: " )
    | opcao == "9" = putStrLn "\nVolte sempre!"
    | otherwise = main


decifraTexto texto chave =
    let textoBinArray = binToBinArrayArray texto
        chaveBytes = stringToBytes chave
        chaveBinArray = bytesToBin chaveBytes
    in decifraBin textoBinArray chaveBinArray

binToBinArrayArray [] = []
binToBinArrayArray texto = take 8 texto : binToBinArrayArray (drop 8 texto)

decifraBin :: [[Integer]] -> [[Integer]] -> [Char]
decifraBin [] _ = ""
decifraBin bytesTexto bytesChave =
   matrizToString (decifraMatriz (carregaMatriz (take 2 bytesTexto)) (carregaMatriz (bytesChave))) ++ decifraBin (drop 2 bytesTexto) (bytesChave)

decifraMatriz :: (Num t, Num t1, Num t2, Num t3, Num t4, Num t5, Ix t, Ix t1, Ix t2, Ix t3, Ix t4, Ix t5) =>
                Array (t4, t5) Integer -> Array (t, t1) Integer -> Array (t2, t3) Integer
decifraMatriz matriz chave1 = 
    let chave2 = expandir chave1 1
        chave3 = expandir chave2 2
    in addRoundKey (chave1) (invSubstituteNibbles $ shiftRows $ invMixColumns $ addRoundKey (chave2) (invSubstituteNibbles $ shiftRows $ addRoundKey chave3 matriz))

cifraTexto :: [Char] -> [Char] -> [Char]
cifraTexto texto chave =
    let textoBytes = stringToBytes texto
        textoBinArray = bytesToBin textoBytes
        chaveBytes = stringToBytes chave
        chaveBinArray = bytesToBin chaveBytes
    in cifraBin textoBinArray chaveBinArray

stringToBytes :: [Char] -> BC.ByteString
stringToBytes texto = BC.pack (padTexto texto)

bytesToBin :: BC.ByteString -> [[Integer]]
bytesToBin = map (pad8Bits . intToBinArray) . B.unpack

cifraBin :: [[Integer]] -> [[Integer]] -> [Char]
cifraBin [] _ = ""
cifraBin bytesTexto bytesChave =
   matrizToString (cifraMatriz (carregaMatriz (take 2 bytesTexto)) (carregaMatriz (bytesChave))) ++ cifraBin (drop 2 bytesTexto) (bytesChave)

carregaMatriz :: (Num t, Num t1, Ix t, Ix t1) => [[Integer]] -> Array (t, t1) Integer
carregaMatriz listaBinarios = 
    array ((1, 1),(2,2)) [((1,1), concatBinario $ take 4 $ listaBinarios!!0), 
                          ((1,2), concatBinario $ drop 4 $ listaBinarios!!0),
                          ((2,1), concatBinario $ take 4 $ listaBinarios!!1),
                          ((2,2), concatBinario $ drop 4 $ listaBinarios!!1)]

matrizToString :: (Num t, Num t1, Ix t, Ix t1) => Array (t, t1) Integer -> [Char]
matrizToString matriz =
    let lista = binToBinArray(matriz!(1,1)) ++ binToBinArray(matriz!(1,2)) ++ binToBinArray(matriz!(2,1)) ++ binToBinArray(matriz!(2,2))
    in concat $ map show lista

--precisa arrendondar depois
base2ToBase10 :: Fractional a => [a] -> a -> a
base2ToBase10 [] _ = 0
base2ToBase10 (x:xs) mult = x * mult + base2ToBase10 xs mult/2

cifraMatriz :: (Num t, Num t1, Num t2, Num t3, Num t4, Num t5, Ix t, Ix t1, Ix t2, Ix t3, Ix t4, Ix t5) =>
                Array (t4, t5) Integer -> Array (t, t1) Integer -> Array (t2, t3) Integer
cifraMatriz matriz chave1 = 
    let chave2 = expandir chave1 1
        chave3 = expandir chave2 2
    in addRoundKey (chave3) (shiftRows $ substituteNibbles $ addRoundKey (chave2) (mixColumns $ shiftRows $ substituteNibbles $ addRoundKey chave1 matriz))

expandir :: (Eq a, Num t, Num t1, Num a, Num t2, Num t3, Ix t, Ix t1, Ix t2, Ix t3) =>
             Array (t, t1) Integer -> a -> Array (t2, t3) Integer
expandir chave round =
    let wA = binToBinArray (chave!(1,1)) ++ binToBinArray (chave!(1,2))
        wB = binToBinArray (chave!(2,1)) ++ binToBinArray (chave!(2,2))
        wC = binToBinArray8Bits $ realizaXorBitPorBit (binToBinArray8Bits $ concatBinario wA) (binToBinArray8Bits $ funcaoG wB round)
        wD = binToBinArray8Bits $ realizaXorBitPorBit (wC) (binToBinArray8Bits $ concatBinario wB) 
        k1 = concatBinario $ take 4 $ wC
        k2 = concatBinario $ drop 4 $ wC
        k3 = concatBinario $ take 4 $ wD
        k4 = concatBinario $ drop 4 $ wD
    in array ((1, 1),(2,2)) [((1,1), k1),
                             ((1,2), k2),
                             ((2,1), k3),
                             ((2,2), k4)]

funcaoG :: (Eq a, Num a) => [Integer] -> a -> Integer
funcaoG bitsWa round =
    let n0 = concatBinario $ take 4 bitsWa
        n1 = concatBinario $ drop 4 bitsWa
    in realizaXorBitPorBit (binToBinArray8Bits $ rCon round) (binToBinArray8Bits $ concatChaves $ (pad4Bits $ binToBinArray $ substituteBitsSbox n1 sBox) ++ (pad4Bits $ binToBinArray $ substituteBitsSbox n0 sBox))

concatChaves :: Show a => [a] -> Integer
concatChaves chaves = (read $ concat $ map (show) chaves) :: Integer

rCon :: (Eq a1, Num a, Num a1) => a1 -> a
rCon round | round == 1 = 10000000
           | round == 2 = 00110000

addRoundKey :: (Num t, Num t1, Num t2, Num t3, Num t4, Num t5, Ix t, Ix t1, Ix t2, Ix t3, Ix t4, Ix t5) =>
                Array (t2, t3) Integer -> Array (t4, t5) Integer -> Array (t, t1) Integer
addRoundKey chave matriz = 
    array ((1, 1),(2,2)) [((1,1), realizaXorBitPorBit (binToBinArray8Bits $ chave!(1,1)) (binToBinArray8Bits $ matriz!(1,1))),
                          ((1,2), realizaXorBitPorBit (binToBinArray8Bits $ chave!(1,2)) (binToBinArray8Bits $ matriz!(1,2))),
                          ((2,1), realizaXorBitPorBit (binToBinArray8Bits $ chave!(2,1)) (binToBinArray8Bits $ matriz!(2,1))),
                          ((2,2), realizaXorBitPorBit (binToBinArray8Bits $ chave!(2,2)) (binToBinArray8Bits $ matriz!(2,2)))]

--Função necessita receber duas listas com 8 elementos inteiros
realizaXorBitPorBit :: [Integer] -> [Integer] -> Integer
realizaXorBitPorBit bitsWa bitsWb' =
    concatBinario $ (bitsWa)!!0 `xor` (bitsWb')!!0 : 
                    (bitsWa)!!1 `xor` (bitsWb')!!1 : 
                    (bitsWa)!!2 `xor` (bitsWb')!!2 : 
                    (bitsWa)!!3 `xor` (bitsWb')!!3 :
                    (bitsWa)!!4 `xor` (bitsWb')!!4 : 
                    (bitsWa)!!5 `xor` (bitsWb')!!5 : 
                    (bitsWa)!!6 `xor` (bitsWb')!!6 : 
                    (bitsWa)!!7 `xor` (bitsWb')!!7 : []

substituteNibbles :: (Num t, Num t1, Num t2, Num t3, Ix t, Ix t1, Ix t2, Ix t3) =>
                      Array (t2, t3) Integer -> Array (t, t1) Integer
substituteNibbles matriz = 
    array ((1, 1),(2,2)) [((1,1), substituteBitsSbox (matriz!(1,1)) sBox),
                          ((1,2), substituteBitsSbox (matriz!(1,2)) sBox),
                          ((2,1), substituteBitsSbox (matriz!(2,1)) sBox),
                          ((2,2), substituteBitsSbox (matriz!(2,2)) sBox)]

invSubstituteNibbles :: (Num t, Num t1, Num t2, Num t3, Ix t, Ix t1, Ix t2, Ix t3) =>
                      Array (t2, t3) Integer -> Array (t, t1) Integer
invSubstituteNibbles matriz =
    array ((1, 1),(2,2)) [((1,1), substituteBitsSbox (matriz!(1,1)) invSBox),
                          ((1,2), substituteBitsSbox (matriz!(1,2)) invSBox),
                          ((2,1), substituteBitsSbox (matriz!(2,1)) invSBox),
                          ((2,2), substituteBitsSbox (matriz!(2,2)) invSBox)]

substituteBitsSbox :: Integer -> Array (Integer, Integer) e -> e
substituteBitsSbox bits box = box!(binArrayToInt(take 2 $ binToBinArray(bits)) + 1, (binArrayToInt(drop 2 $ binToBinArray(bits))+1)) 

binArrayToInt::[Integer] -> Integer
binArrayToInt bits = bits!!0 * 2 + bits!!1 * 1

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

shiftRows :: (Num t, Num t1, Num t2, Num t3, Ix t, Ix t1, Ix t2, Ix t3) => Array (t2, t3) e -> Array (t, t1) e
shiftRows matriz =
    array ((1, 1),(2,2))  [((1,1), matriz!(1,1)), ((1,2), matriz!(1,2)),
                           ((2,1), matriz!(2,2)), ((2,2), matriz!(2,1))]

mixColumns :: (Num t, Num t1, Num t2, Num t3, Ix t, Ix t1, Ix t2, Ix t3) => Array (t2, t3) Integer -> Array (t, t1) Integer
mixColumns matriz =
    array ((1, 1),(2,2)) [((1,1), realizaXorBitPorBit (binToBinArray8Bits $ multiplicaPor2 $ multiplicaPor2 (matriz!(2,1)))  (binToBinArray8Bits (matriz!(1,1)))), 
                          ((1,2), realizaXorBitPorBit (binToBinArray8Bits $ multiplicaPor2 $ multiplicaPor2 (matriz!(2,2)))  (binToBinArray8Bits (matriz!(1,2)))),
                          ((2,1), realizaXorBitPorBit (binToBinArray8Bits $ multiplicaPor2 $ multiplicaPor2 (matriz!(1,1)))  (binToBinArray8Bits (matriz!(2,1)))),
                          ((2,2), realizaXorBitPorBit (binToBinArray8Bits $ multiplicaPor2 $ multiplicaPor2 (matriz!(1,2)))  (binToBinArray8Bits (matriz!(2,2))))]

invMixColumns :: (Num t, Num t1, Num t2, Num t3, Ix t, Ix t1, Ix t2, Ix t3) => Array (t2, t3) Integer -> Array (t, t1) Integer
invMixColumns matriz =
    array ((1, 1),(2,2)) [((1,1), realizaXorBitPorBit (binToBinArray8Bits $ multiplicaPor2 (matriz!(2,1)))  (binToBinArray8Bits $ realizaXorBitPorBit (binToBinArray8Bits (matriz!(1,1))) ( binToBinArray8Bits $ multiplicaPor2 $ multiplicaPor2 $ multiplicaPor2 (matriz!(1,1)) ) )), 
                          ((1,2), realizaXorBitPorBit (binToBinArray8Bits $ multiplicaPor2 (matriz!(2,2)))  (binToBinArray8Bits $ realizaXorBitPorBit (binToBinArray8Bits (matriz!(1,2))) ( binToBinArray8Bits $ multiplicaPor2 $ multiplicaPor2 $ multiplicaPor2 (matriz!(1,2)) ) )),
                          ((2,1), realizaXorBitPorBit (binToBinArray8Bits $ multiplicaPor2 (matriz!(1,1)))  (binToBinArray8Bits $ realizaXorBitPorBit (binToBinArray8Bits (matriz!(2,1))) ( binToBinArray8Bits $ multiplicaPor2 $ multiplicaPor2 $ multiplicaPor2 (matriz!(2,1)) ) )),
                          ((2,2), realizaXorBitPorBit (binToBinArray8Bits $ multiplicaPor2 (matriz!(1,2)))  (binToBinArray8Bits $ realizaXorBitPorBit (binToBinArray8Bits (matriz!(2,2))) ( binToBinArray8Bits $ multiplicaPor2 $ multiplicaPor2 $ multiplicaPor2 (matriz!(2,2)) ) ))]

multiplicaPor2 :: Integer -> Integer
multiplicaPor2 valor
    | valor < 1000 = valor * 10
    | otherwise = realizaXorBitPorBit (binToBinArray8Bits (valor*10-10000)) (binToBinArray8Bits 0011)

intToBinArray :: (Integral a1, Num a) => a1 -> [a]
intToBinArray n = reverse (base10ToBase2 n)

base10ToBase2 :: (Integral a, Num t) => a -> [t]
base10ToBase2 0 = []
base10ToBase2 n | n `mod` 2 == 1 = 1 : base10ToBase2 (n `div` 2)
                | n `mod` 2 == 0 = 0 : base10ToBase2 (n `div` 2)

padTexto :: [Char] -> [Char]
padTexto texto = texto ++ replicate ((length texto) `mod` 2) ' '

concatBinario :: [Integer] -> Integer
concatBinario = read . concatMap show

sBox = array ((1,1),(4,4)) [((1,1), 1001), ((1,2), 0100), ((1,3), 1010), ((1,4), 1011),
                            ((2,1), 1101), ((2,2), 0001), ((2,3), 1000), ((2,4), 0101),
                            ((3,1), 0110), ((3,2), 0010), ((3,3), 0000), ((3,4), 0011),
                            ((4,1), 1100), ((4,2), 1110), ((4,3), 1111), ((4,4), 0111)]

invSBox = 
  array ((1,1),(4,4)) [ ((1,1), 1010), ((1,2), 0101), ((1,3), 1001), ((1,4), 1011),
                        ((2,1), 0001), ((2,2), 0111), ((2,3), 1000), ((2,4), 1111),
                        ((3,1), 0110), ((3,2), 0000), ((3,3), 0010), ((3,4), 0011),
                        ((4,1), 1100), ((4,2), 0100), ((4,3), 1101), ((4,4), 1110)]

chave = array ((1, 1),(2,2)) [((1,1), 0010), 
                              ((1,2), 1101),
                              ((2,1), 0101),
                              ((2,2), 0101)]

meuArray =  array ((1, 1),(2,2)) [((1,1), 0110), 
                                  ((1,2), 0100),
                                  ((2,1), 1100),
                                  ((2,2), 0000)]

