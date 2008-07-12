module HigherBrain where
import Memory
import Types
import Control.Concurrent.MVar

calculateTargetPoint :: Memory -> MVar Point -> IO ()
calculateTargetPoint mem ptMV = swapMVar ptMV (0, 0) >> return ()
