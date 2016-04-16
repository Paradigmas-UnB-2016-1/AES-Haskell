import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Array

bytestring = BC.pack "dudu moreira"

bytes = B.unpack bytestring
chars = BC.unpack bytestring

main = do
    print bytes
    print chars
    print $ elems myArray
    print $ assocs myArray
    let primeiro = bytes !! 1 in print $ primeiro:[1,2]
    print $ bytes !! 1

myArray :: Array (Int, Int) Char
myArray = array ((1, 1),(4,4)) [((1,1), ' '), ((1,2), ' '),((1,3), ' '), ((1,4), ' '),
                                ((2,1), ' '), ((2,2), ' '),((2,3), ' '), ((2,4), ' '),
                                ((3,1), ' '), ((3,2), ' '),((3,3), ' '), ((3,4), ' '),
                                ((4,1), ' '), ((4,2), ' '),((4,3), ' '), ((4,4), ' ')]