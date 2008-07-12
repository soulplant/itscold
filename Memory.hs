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

updateTelem :: Telemetry -> Memory -> Memory
updateTelem telem mem = mem {memObjects = newObjects, memVehicleState = newVehicleState}
  where
    newObjects      = (fromList $ objects telem) `union` memObjects mem
    newVehicleState = vehicleState telem
