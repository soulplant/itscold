module Main where
import Types
import Memory
import HigherBrain
import LowerBrain
import MessageParser
import System.IO
import Prelude hiding(Left, Right)
import Control.Concurrent.MVar
import Control.Concurrent
import System.IO.Unsafe
import Network.Socket

-- Starts the various threads in the system.
startThreads :: IO ()
startThreads = do
  memoryMV <- newEmptyMVar
  h <- getNetworkHandle
  forkIO (updateMemoryThread memoryMV h)

  targetPointMV <- newMVar (0,0)
  -- forkIO (callHigherBrainThread memoryMV targetPointMV)
  forkIO (sendMessageThread h memoryMV targetPointMV)

  forever (return ())

callHigherBrainThread memMV targetPointMV = forever $ do
  mem <- readMVar memMV
  targetPoint <- readMVar targetPointMV
  -- Write the targetPoint MVar.
  let targetPoint' = calculateTargetPoint mem targetPoint
  takeMVar targetPointMV
  putMVar targetPointMV targetPoint'

sendMessageThread h memMV targetPointMV = forever $ do
  mem <- readMVar memMV
  targetPoint <- readMVar targetPointMV
  -- Send the command to the server.
  hSendMessage h (getCommand mem targetPoint)

main = do
  hSetBuffering stdout NoBuffering
  startThreads

forever m = m >> forever m

-- The thread that listens for network input and updates the memory.
updateMemoryThread :: MVar Memory -> Handle -> IO ()
updateMemoryThread memoryMV h = forever $ do
  msg <- getMessage (hGetChar h)
  case msg of
    (Init initMessage) -> createOrInitializeMemory memoryMV initMessage
    (Telem telemData)  -> liftMemUpdate $ updateTelem telemData
    _                  -> return ()

  where
    createOrInitializeMemory mv init = do
      maybeMem <- tryTakeMVar mv

      case maybeMem of
        Just mem -> putMVar mv (mem{ memTrialInfo = init })
        Nothing  -> putMVar mv (mkMemory init)

    liftMemUpdate t = do
      m <- takeMVar memoryMV
      putMVar memoryMV (t m)

getMessage :: IO Char -> IO Message
getMessage getc = getMessage' getc ""

getMessage' :: IO Char -> String -> IO Message
getMessage' getc str = do
  c <- getc
  if (c == ';') then return (parseMessage (reverse str))
                else getMessage' getc (c:str)

e c = hPutStr stderr [c,';']
-- |Send a message to the serve
hSendMessage :: Handle -> VehicleControl -> IO ()
hSendMessage h vc = do
                  let accStr = sendAcc $ vcAcc vc
                  let dirStr = sendDir $ vcDir vc
                  let sendStr = accStr ++ dirStr
                  if (sendStr == "") then return () else lPuts (sendStr ++ ";")
  where
    sendAcc Accelerate = "a"
    sendAcc Brake      = "b"
    sendAcc Roll       = ""

    sendDir HardLeft  = "l;l"
    sendDir Left      = "l"
    sendDir Straight  = ""
    sendDir Right     = "r"
    sendDir HardRight = "r;r"


    lPuts str = do
      -- hPutStrLn stderr ("sending: " ++ str)
      hPutStr h str
      hFlush h

getNetworkHandle :: IO Handle
getNetworkHandle = do
  sock <- socket AF_INET Stream defaultProtocol
  hostAddr <- inet_addr "127.0.0.1"
  connect sock (SockAddrInet 17676 hostAddr)
  result <- socketToHandle sock ReadWriteMode
  hSetBuffering result LineBuffering
  return result
