module LowerBrain where

import Prelude hiding (Left, Right)
import Types
import Memory
import Debug.Trace

turnThresh  = 5
accelThresh = 10

getCommand :: Memory -> Point -> VehicleControl
getCommand mem pt = VC (accelCommand mem pt) (turnCommand mem pt)

accelCommand :: Memory -> Point -> Acceleration
accelCommand mem targetPoint
  | diff360 angleToHome vd < accelThresh = Accelerate
  | otherwise                            = Roll
  where
    vs          = memVehicleState mem
    vd          = vehicleDir vs
    angleToHome = calcAngle (vehicleX vs, vehicleY vs) targetPoint

turnCommand :: Memory -> Point -> Direction
turnCommand mem targetPoint
  | (reduce360 angleToHome) - (reduce360 vd) > turnThresh  = Left
  | (reduce360 angleToHome) - (reduce360 vd) < -turnThresh = Right
  | otherwise                                              = Straight
  where
    vd          = vehicleDir vs
    vs          = memVehicleState mem
    angleToHome = calcAngle (vehicleX vs, vehicleY vs) targetPoint

diff360 :: Double -> Double -> Double
diff360 d1 d2 = reduce360 (d1 - d2)

reduce360 :: Double -> Double
reduce360 deg
  | deg > 360 = reduce360 $ deg - 360
  | deg < 0   = reduce360 $ deg + 360
  | otherwise = deg

calcAngle :: Point -> Point -> Double
calcAngle (x1,y1) (x2,y2)
  | (x2 >= x1) && (y2 >= y1) = baseAngle
  | (x2 < x1)  && (y2 >= y1) = 180 - baseAngle
  | (x2 < x1)  && (y2 < y1)  = 180 + baseAngle
  | otherwise                = 360 - baseAngle
  where
    baseAngle = abs (atan ((y2-y1)/(x2-x1)) / pi * 180)
