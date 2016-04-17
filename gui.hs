import Graphics.UI.Gtk
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window [windowTitle := "AES - Criptografia", containerBorderWidth := 10]

  vb <- vBoxNew False 15
  containerAdd window vb

  hb <- hBoxNew False 15
  boxPackStart vb hb PackNatural 0

  txtfield <- entryNew
  boxPackStart hb txtfield PackNatural 50

  txtstack <- statusbarNew
  boxPackStart vb txtstack PackNatural 0
  id <- statusbarGetContextId txtstack "Line"

  widgetShowAll window

  onEntryActivate txtfield (saveText txtfield txtstack id)

  onDestroy window mainQuit
  mainGUI

saveText :: Entry -> Statusbar -> ContextId -> IO ()

saveText fld stk id = do

    txt <- entryGetText fld

    bytestring = BC.pack txt

    bytes = B.unpack bytestring

    r = head bytes

    
calculaBinario :: (Integral a, Num t) => a -> [t]
calculaBinario 0 = []
calculaBinario n | n `mod` 2 == 1 = 1 : calculaBinario (n `div` 2)
                 | n `mod` 2 == 0 = 0 : calculaBinario (n `div` 2)

e = calculaBinario r


    let mesg  txt ++ " Texto criptografado: " ++ e
    msgid <- statusbarPush stk id mesg

    return ()
