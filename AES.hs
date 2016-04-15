import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import System.IO
import System.Exit
import Control.Monad

main = forever (printMenu >> readChoice >>= menuAction)

printMenu = putStr "\n\n\ne)ncriptar'\ns)air\nOpção: " >> hFlush stdout

readChoice = hSetBuffering stdin NoBuffering >> hSetEcho stdin False >> getChar

menuAction 'e' = encriptar
menuAction 's' = exitSuccess
menuAction _ = hPutStrLn stderr "\n\n\nEscolha inválida"
 
deserialiseHeader :: Get (Word32, Word32, Word32)
deserialiseHeader = do
  a00 <- getWord32be
  a01 <- getWord32be
  a02 <- getWord32be
  return (a00, a01, a02)

encriptar :: IO ()
encriptar = do
  putStr "Digite o texto para ser encriptado: \n"
  texto <- readLn
  putStrLn( show (read texto))
  print $ runGet deserialiseHeader texto
