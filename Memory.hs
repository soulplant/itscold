module Memory where
import Types
import Data.Set

data Memory = Memory {
  memTrialInfo   :: Initialization,
  memVehicleState :: VehicleState,
  memObjects      :: Set Object,
  memMartians     :: Set Martian
}
  deriving (Show)

mkMemory :: Initialization -> Memory
mkMemory trialInfo = Memory { memTrialInfo = trialInfo, memObjects = empty, memVehicleState = defaultVS, memMartians = empty }

updateMemory :: Message -> Memory -> Memory
updateMemory msg mem = mem {memObjects = newObjects}
  where
    newObjects = (fromList $ objects telem) `union` memObjects mem
    telem = case msg of (Telem t) -> t
