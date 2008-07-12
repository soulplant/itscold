module Types where

-- Messages are what we get from the server
data Message = Init Initialization
             | Telem Telemetry
             | Bounce
             | CraterKill
             | MartianKill
             | Success
             | End
  deriving (Show, Eq)

-- ADT For Initialization data
data Initialization = I {
    dx :: Double,
    dy :: Double,
    timeLimit :: Int,
    minSensor :: Double,
    maxSensor :: Double,
    maxSpeed :: Double,
    maxTurn :: Double,
    maxHardTurn :: Double
  }
  deriving (Show, Eq)

-- ADT For Telemetry data
data Telemetry = T {
    timeStamp :: Int,
    vehicleState :: VehicleState,
    objects :: [Object],
    martians :: [Martian]
  }
  deriving (Show, Eq)

data VehicleState = VS {
  vehicleCtl :: VehicleControl,
  vehicleX :: Double,
  vehicleY :: Double,
  vehicleDir :: Double,
  vehicleSpeed :: Double
}
  deriving (Show, Eq)

data VehicleControl = VC {
  vcAcc :: Acceleration,
  vcDir :: Direction
}
  deriving (Show, Eq)

data Acceleration = Accelerate | Brake | Roll
  deriving (Show, Eq)

data Direction    = HardLeft | Left | Straight | Right | HardRight
  deriving (Show, Eq)

-- Stationary things in the terain
data Object =
  Object {
    objectKind :: ObjectKind,
    objectX :: Double,
    objectY :: Double,
    objectR :: Double
  }
  deriving (Show, Eq, Ord)

data ObjectKind = Boulder | Crater | Home
  deriving (Show, Eq, Ord)

data Martian = Martian {
    martianX :: Double,
    martianY :: Double,
    martianDir :: Double,
    martianSpeed :: Double
  }
  deriving (Show, Eq, Ord)

type Point = (Double, Double)

defaultVS = VS defaultVC 0 0 0 0

-- Default values
emptyInit  = I 0 0 0 0 0 0 0 0 
emptyTelem = T 0 defaultVS [] []

defaultVC    = VC Roll Straight
