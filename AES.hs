import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Array

bytestring = BC.pack "dudu"

bytes = B.unpack bytestring
chars = BC.unpack bytestring

main = do
    print bytes
    print chars
    let primeiro = bytes !! 0 in print $ toBin primeiro
    print $ elems matriz
    print $ assocs matriz
    let primeiro = bytes !! 0 in print $ primeiro:[1,2]

toBin :: (Integral a, Num t) => a -> [t]
toBin 0 = [0]
toBin n = reverse (helper n)

helper :: (Integral a, Num t) => a -> [t]
helper 0 = []
helper n | n `mod` 2 == 1 = 1 : helper (n `div` 2)
         | n `mod` 2 == 0 = 0 : helper (n `div` 2)

matriz :: Array (Int, Int) Int
matriz = array ((1, 1),(2,2)) [((1,1), 0), ((1,2), 0),
                                ((2,1), 0), ((2,2), 0)]

