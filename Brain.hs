module Brain where

import Types
import Memory

turnThresh  = 0
accelThresh = 10

getCommand :: Memory -> VehicleControl
getCommand _ = VC Accelerate Straight

calcAngle :: (Double, Double) -> (Double, Double) -> Double
calcAngle (x1,y1) (x2,y2)
  | (x2 >= x1) && (y2 >= y1) = baseAngle
  | (x2 < x1)  && (y2 >= y1) = 180 - baseAngle
  | (x2 < x1)  && (y2 < y1)  = 180 + baseAngle
  | otherwise                = 360 - baseAngle
  where
    baseAngle = abs (atan ((y2-y1)/(x2-x1)) / pi * 180)
