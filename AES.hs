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
    putStr "Digite a opção desejada:    "
    opcao <- getLine
    menu opcao
    putStrLn ""

menu :: [Char] -> IO ()
menu opcao 
    | opcao == "2" = putStrLn "Decifrar"
    | opcao == "9" = putStrLn "Volte sempre!"
    | otherwise = main

--printCifraTexto =
--    let texto = recebeTexto
--        bytes = bytesTexto texto
--        binArrays = textoToBinArray bytes
--    in putStrLn (cifraTexto bytesTexto bytesChave)

recebeTexto = do
    putStr "Digite o texto para ser cifrado: "
    palavra <- getLine
    return palavra

bytesTexto :: [Char] -> BC.ByteString
bytesTexto texto = BC.pack (padTexto texto)

textoToBinArray = map (concatBinario . intToBinArray) . B.unpack

byteStringChave = BC.pack "*S"

bytesChave = B.unpack byteStringChave
--charsTexto = BC.unpack byteStringTexto

cifraTexto [] _ = ""
cifraTexto bytesTexto bytesChave =
   matrizToString (cifraMatriz (carregaMatriz (take 4 bytesTexto)) (carregaMatriz (bytesChave))) ++ cifraTexto (drop 4 bytesTexto) (bytesChave)

carregaMatriz listaBinarios = 
    array ((1, 1),(2,2)) [((1,1), listaBinarios!!0), 
                          ((1,2), listaBinarios!!1),
                          ((2,1), listaBinarios!!2),
                          ((2,2), listaBinarios!!3)]

matrizToString :: (Num t, Num t1, Ix t, Ix t1) => Array (t, t1) Integer -> String
matrizToString matriz =
    show $ concatBinario $ matriz!(1,1):matriz!(1,2):[]

--precisa arrendondar depois
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
    in realizaXorBitPorBit (binToBinArray8Bits $ rCon round) (binToBinArray8Bits $ concatChaves $ (pad4Bits $ binToBinArray $ substituiBitsSbox n1) ++ (pad4Bits $ binToBinArray $ substituiBitsSbox n0))

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
    array ((1, 1),(2,2)) [((1,1), substituiBitsSbox $ matriz!(1,1)),
                          ((1,2), substituiBitsSbox $ matriz!(1,2)),
                          ((2,1), substituiBitsSbox $ matriz!(2,1)),
                          ((2,2), substituiBitsSbox $ matriz!(2,2))]

substituiBitsSbox :: Integer -> Integer
substituiBitsSbox bits = sBox!(binArrayToInt(take 2 $ binToBinArray(bits)) + 1, (binArrayToInt(drop 2 $ binToBinArray(bits))+1)) 

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

multiplicaPor2 :: Integer -> Integer
multiplicaPor2 valor
    | valor < 1000 = valor * 10
    | otherwise = realizaXorBitPorBit (binToBinArray8Bits (valor*10-10000)) (binToBinArray8Bits 0011)

--intToBinArray :: (Integral a, Num t) => a -> [t]
--intToBinArray 0 = [0]
intToBinArray n = reverse (base10ToBase2 n)

base10ToBase2 :: (Integral a, Num t) => a -> [t]
base10ToBase2 0 = []
base10ToBase2 n | n `mod` 2 == 1 = 1 : base10ToBase2 (n `div` 2)
                | n `mod` 2 == 0 = 0 : base10ToBase2 (n `div` 2)

padTexto :: [Char] -> [Char]
padTexto texto = texto ++ replicate ((length texto) `mod` 2) ' '

concatBinario :: [Integer] -> Integer
concatBinario = read . concatMap show

--polinomio2por2 = array ((1, 1),(2,2)) [((1,1), 0001), ((1,2), 0100),
--                                       ((2,1), 0100), ((2,2), 0001)]

sBox = array ((1,1),(4,4)) [((1,1), 1001), ((1,2), 0100), ((1,3), 1010), ((1,4), 1011),
                            ((2,1), 1101), ((2,2), 0001), ((2,3), 1000), ((2,4), 0101),
                            ((3,1), 0110), ((3,2), 0010), ((3,3), 0000), ((3,4), 0011),
                            ((4,1), 1100), ((4,2), 1110), ((4,3), 1111), ((4,4), 0111)]

--sBoxInversa = 
--  array ((1,1),(4,4)) [ ((1,1), ), ((1,2), ), ((1,3), ), ((1,4), ),
--               ((2,1), ), ((2,2), ), ((2,3), ), ((2,4), ),
--               ((3,1), ), ((3,2), ), ((3,3), ), ((3,4), ),
--               ((4,1), ), ((4,2), ), ((4,3), ), ((4,4), )]

chave = array ((1, 1),(2,2)) [((1,1), 0010), 
                              ((1,2), 1101),
                              ((2,1), 0101),
                              ((2,2), 0101)]

meuArray =  array ((1, 1),(2,2)) [((1,1), 0110), 
                                  ((1,2), 0100),
                                  ((2,1), 1100),
                                  ((2,2), 0000)]

