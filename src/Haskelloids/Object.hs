{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}

module Haskelloids.Object
  ( Object,
    ObjectClass (..),
    ObjectInput (..),
    ObjectOutput (..),
    teleport,
    reload,
    hits,
  )
where

import Control.Arrow ((&&&), returnA)
import Data.IdentityList (ILKey)
import FRP.Yampa
  ( (>>>),
    (>>^),
    DTime,
    Event (..),
    SF,
    after,
    attach,
    constant,
    identity,
    integral,
    merge,
    once,
    switch,
  )
import Graphics.HGL (Graphic)
import Haskelloids.Geometry (Point2, Shape, intersect)
import Haskelloids.Input (UserInput)

-- #### Data type definitions ##################################################

-- ## Game objects ##############################

type Object = SF ObjectInput ObjectOutput

data ObjectInput = ObjectInput
  { oiUserInput :: !UserInput,
    oiHit :: Event ()
  }

data ObjectClass
  = Asteroid
  | Bullet
  | Ship
  | Dust
  deriving (Eq)

-- ObjectOutput - a common interface for the drawing of the game state plus the addition and removal of game objects
data ObjectOutput = ObjectOutput
  { ooPos :: Point2,
    ooCllsnBox :: !Shape,
    ooGraphic :: !Graphic,
    ooSpawnReq :: Event [Object],
    ooObjClass :: ObjectClass,
    ooKillReq :: Event ()
  }

-- #### Signal functions #######################################################

-- teleport - auxiliary signal function that wraps co-ordinates round in a one-dimensional co-ordinate system with a fixed buffer size
teleport :: Int -> Int -> Double -> SF Double Double
teleport sz buf x0 = switch (init' &&& (init' >>> evt)) (uncurry (teleport sz buf .))
  where
    init' :: SF Double Double
    init' = integral >>^ (x0 +)
    evt :: SF Double (Event (Double -> Double, Double))
    evt = proc x -> do
      let sz' = fromIntegral sz
          buf' = fromIntegral buf
          lt = (\d -> if d then Event (2 * buf' + sz' +) else NoEvent) . (< negate buf') $ x
          gt = (\d -> if d then Event ((2 * buf' + sz') `subtract`) else NoEvent) . (> sz' + buf') $ x
      returnA -< flip attach x . merge lt $ gt

-- reload - auxiliary signal function that yields an Event on the first Event to arrive and then waits the specified interval until yielding another Event again
reload :: DTime -> SF (Event ()) (Event ())
reload intvl = proc e -> do
  switch (constant NoEvent &&& identity) (const pause) -< e
  where
    pause :: SF (Event ()) (Event ())
    pause = switch (once &&& after intvl ()) (\_ -> reload intvl)

-- ##### Function definitions #################################################

-- hits - calculates which objects have collided with another and returns them
hits :: [(ILKey, ObjectOutput)] -> [ILKey]
hits objs = hits' objs []

hits' :: [(ILKey, ObjectOutput)] -> [(ILKey, ObjectOutput)] -> [ILKey]
hits' [] _ = []
hits' ((k, oo) : rest) seen =
  let cllsn =
        any
          ( \x ->
              (collideObj (ooObjClass oo) . ooObjClass . snd $ x)
                && (intersect (ooCllsnBox oo) . ooCllsnBox . snd $ x)
          )
          (seen ++ rest)
   in if cllsn
        then k : hits' rest ((k, oo) : seen)
        else hits' rest ((k, oo) : seen)

-- collideObj - collision function
collideObj :: ObjectClass -> ObjectClass -> Bool
collideObj Dust _ = False
collideObj _ Dust = False
collideObj Asteroid Asteroid = False
collideObj Asteroid _ = True
collideObj Ship Bullet = False
collideObj Ship _ = True
collideObj Bullet Ship = False
collideObj Bullet _ = True
