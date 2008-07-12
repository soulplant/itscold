module MessageParser where

import Text.ParserCombinators.Parsec
import qualified Types as T

match c p = char c >> spaces >> return p

parseMessage s = fromRight $ parse message "" s

message = initialization <|> telemetry <|> bounce <|> craterKill <|> martianKill <|> successMessage <|> endMessage

-- Note: These are wrong because we don't consume all the input for each of
-- these messages, but because we only parse what we can and throw the rest
-- away, we never get burned.
bounce = match 'B' T.Bounce
craterKill = match 'C' T.CraterKill
martianKill = match 'K' T.MartianKill
successMessage = match 'S' T.Success
endMessage = match 'E' T.End

initialization = do
  match 'I' ()
  dx <- double
  dy <- double
  timeLimit <- int
  minSensor <- double
  maxSensor <- double
  maxSpeed  <- double
  maxTurn   <- double
  maxHardTurn <- double
  return $ T.Init (T.I {
      T.dx = dx,
      T.dy = dy,
      T.timeLimit = timeLimit,
      T.minSensor = minSensor,
      T.maxSensor = maxSensor,
      T.maxSpeed = maxSpeed,
      T.maxTurn = maxTurn,
      T.maxHardTurn = maxHardTurn})
  
telemetry
  = do
      match 'T' ()
      timeStamp <- int
      vehicleSt <- vehicleState
      things <- many objOrMartian
      let martians = [fromLeft x | x <- things, isLeft x]
      let objects  = [fromRight x | x <- things, isRight x]
      return $ T.Telem (T.T {
          T.timeStamp    = timeStamp,
          T.vehicleState = vehicleSt,
          T.objects  = objects,
          T.martians = martians})

vehicleState = do
  vehicleCtl <- vehicleControl
  vehicleX <- double
  vehicleY <- double
  vehicleDir <- double
  vehicleSpeed <- double
  return $ T.VS {
    T.vehicleCtl = vehicleCtl,
    T.vehicleX = vehicleX,
    T.vehicleY = vehicleY,
    T.vehicleDir = vehicleDir,
    T.vehicleSpeed = vehicleSpeed}

  
vehicleControl
  = do
      vcAcc <- acceleration
      vcDir <- direction
      return $ T.VC {
        T.vcAcc = vcAcc,
        T.vcDir = vcDir}

acceleration =  match 'a' T.Accelerate
            <|> match 'b' T.Brake
            <|> match '-' T.Roll

direction =  match 'L' T.HardLeft
         <|> match 'l' T.Left
         <|> match '-' T.Straight
         <|> match 'r' T.Right
         <|> match 'R' T.HardRight




objOrMartian = martian <+> object

object
  = do        
      kind <- objectKind
      objectX <- double
      objectY <- double
      objectR <- double
      return $ T.Object {
          T.objectKind = kind,
          T.objectX = objectX,
          T.objectY = objectY,
          T.objectR = objectR}

objectKind =  match 'b' T.Boulder
          <|> match 'c' T.Crater
          <|> match 'h' T.Home

martian = do
  match 'm' ()
  martianX <- double
  martianY <- double
  martianDir <- double
  martianSpeed <- double
  return $ T.Martian {
      T.martianX = martianX,
      T.martianY = martianY,
      T.martianDir = martianDir,
      T.martianSpeed = martianSpeed}

double = do
          sign <- option "" (string "-")
          ds1 <- many1 digit
          char '.'
          ds2 <- many1 digit
          spaces
          return $ (read (sign ++ ds1 ++ "." ++ ds2)::Double)

int = do
       sign <- option "" (string "-")
       ds <- many1 digit
       spaces
       return $ (read (sign ++ ds)::Int)


p1 <+> p2 = (p1 >>= (return . Left)) <|> (p2  >>= (return . Right))


fromLeft (Left x)   = x
fromRight (Right x) = x

isLeft (Left _) = True
isLeft (Right _) = False
isRight = not . isLeft
