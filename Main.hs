--hi

module Main where
import qualified Types as T
import System.IO
import Prelude
import Text.ParserCombinators.Parsec

main = do
  hSetBuffering stdout NoBuffering
  msg <- getMessage
  sendMessage T.emptyVC{T.vcAcc = T.Accelerate}
  main
  
getMessage :: IO T.Message
getMessage = getMessage' ""
getMessage' str = do
  c <- getChar
  if (c == ';') then return (parseMessage (reverse str))
                else getMessage' (c:str)

parseMessage _ = T.emptyTelem

p_message = do
              c <- anyChar
              spaces
              return $ case c of
                'I' -> p_initMessage
                'T' -> p_telemetryMessage

p_initMessage = do 
                  dx <- p_double
                  return T.emptyInit

p_telemetryMessage = do
                      anyChar
                      return T.emptyTelem


-- |Send a message to the serve
sendMessage :: T.VehicleControl -> IO ()
sendMessage vc = do
                  sendAcc $ T.vcAcc vc
                  sendDir $ T.vcDir vc
                  putStr ";"
  where
    sendAcc T.Accelerate = putStr "a"
    sendAcc T.Brake      = putStr "b"
    sendAcc T.Roll       = return ()

    sendDir T.HardLeft  = putStr "l;l"
    sendDir T.Left      = putStr "l"
    sendDir T.Straight  = return ()
    sendDir T.Right     = putStr "r"
    sendDir T.HardRight = putStr "r;r"
