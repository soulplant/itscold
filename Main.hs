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

targetPoint :: MVar Point
targetPoint = unsafePerformIO (newMVar (0,0))

main' :: MVar Memory -> (String -> IO ()) -> IO ()
main' memoryMV puts = do
  mem <- readMVar memoryMV

  -- Write the targetPoint MVar.
  calculateTargetPoint mem targetPoint

  tp <- readMVar targetPoint

  -- Send the command to the server.
  sendMessage (getCommand mem tp) puts

  let cmd = getCommand mem tp


  main' memoryMV puts

-- Starts the various threads in the system.
startThreads :: IO ()
startThreads = do
  memoryMV <- newEmptyMVar
  (networkGetChar, networkPutString) <- f
  forkIO (updateMemoryThread memoryMV networkGetChar)
  main' memoryMV networkPutString

main = do
  hSetBuffering stdout NoBuffering
  startThreads

forever m = m >> forever m

-- The thread that listens for network input and updates the memory.
updateMemoryThread :: MVar Memory -> IO Char -> IO ()
updateMemoryThread memoryMV getc = forever $ do
  msg <- getMessage getc
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
      putStrLn "Received a message."
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
sendMessage :: VehicleControl -> (String -> IO ()) -> IO ()
sendMessage vc puts = do
                  sendAcc $ vcAcc vc
                  sendDir $ vcDir vc
                  lPuts "o;"
  where
    sendAcc Accelerate = lPuts "a"
    sendAcc Brake      = lPuts "b"
    sendAcc Roll       = return ()

    sendDir HardLeft  = lPuts "l;l"
    sendDir Left      = lPuts "l"
    sendDir Straight  = return ()
    sendDir Right     = lPuts "r"
    sendDir HardRight = lPuts "r;r"


    lPuts str = do
      hPutStrLn stderr ("sending: " ++ str)
      puts str


f :: IO Handle
f = do
  sock <- socket AF_INET Stream defaultProtocol
  hostAddr <- inet_addr "127.0.0.1"
  connect sock (SockAddrInet 17676 hostAddr)
  return (networkGetChar sock, networkPutStr sock)
  where
    networkGetChar sock = (recv sock 1) >>= return . head
    networkPutStr sock str = send sock str >> return ()
