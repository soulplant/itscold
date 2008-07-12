module Main where
import Types
import Memory
import Brain
import MessageParser
import System.IO
import Prelude hiding(Left, Right)

main' :: Memory -> IO ()
main' memory = do
  hSetBuffering stdout NoBuffering
  msg <- getMessage
  let memory' = updateMemory msg memory
  sendMessage (getCommand memory')
  main' memory'
  
main :: IO ()
main = do
  let msg = emptyInit
  main' (mkMemory msg)
  
getMessage :: IO Message
getMessage = getMessage' ""

getMessage' :: String -> IO Message
getMessage' str = do
  c <- getChar
  if (c == ';') then return (parseMessage (reverse str))
                else getMessage' (c:str)

-- |Send a message to the serve
sendMessage :: VehicleControl -> IO ()
sendMessage vc = do
                  sendAcc $ vcAcc vc
                  sendDir $ vcDir vc
                  putStr ";"
  where
    sendAcc Accelerate = putStr "a"
    sendAcc Brake      = putStr "b"
    sendAcc Roll       = return ()

    sendDir HardLeft  = putStr "l;l"
    sendDir Left      = putStr "l"
    sendDir Straight  = return ()
    sendDir Right     = putStr "r"
    sendDir HardRight = putStr "r;r"
