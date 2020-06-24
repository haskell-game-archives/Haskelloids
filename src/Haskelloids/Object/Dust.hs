{-# LANGUAGE Arrows #-}

module Haskelloids.Object.Dust
  ( dustSF,
    makeDust,
  )
where

import Control.Arrow (returnA)
import Control.Monad.Random (Rand, RandomGen, getRandomR)
import FRP.Yampa (Event (..), (^<<), after, integral)
import Haskelloids.Geometry (Angle, Figure (..), Point2, origin, shape)
import Haskelloids.Graphics (drawFigure)
import Haskelloids.Object (Object, ObjectClass (..), ObjectOutput (..))

-- #### Program constants ######################################################

speed :: Double
speed = 200

minAge, maxAge :: Double
minAge = 0.1
maxAge = 0.5

figure :: Figure
figure = Circle origin 1

-- #### Signal functions #######################################################

dustSF :: Point2 -> Angle -> Double -> Object
dustSF (x0, y0) o age = proc _ -> do
  let vx = speed * cos o
      vy = speed * sin o

  x <- (x0 +) ^<< integral -< vx
  y <- (y0 +) ^<< integral -< vy

  die <- after age () -< ()

  returnA
    -<
      ObjectOutput
        { ooPos = (x, y),
          ooCllsnBox = shape figure, -- doesn't collide with anything
          ooGraphic = drawFigure . Translate (x, y) $ figure,
          ooSpawnReq = NoEvent,
          ooObjClass = Dust,
          ooKillReq = die
        }

-- #### Function definitions ###################################################

makeDust :: RandomGen g => Point2 -> Rand g Object
makeDust (x, y) = do
  age <- getRandomR (minAge, maxAge)
  x0 <- (x +) <$> getRandomR (-10.0, 10)
  y0 <- (y +) <$> getRandomR (-10.0, 10)
  o <- getRandomR (0, 2 * pi)

  return (dustSF (x0, y0) o age)
