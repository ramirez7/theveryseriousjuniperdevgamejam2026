module MJ626.Physics where

import MJ626.ECS
import Linear
or0 :: Num a => Maybe a -> a
or0 = maybe 0 id

applyPhysics
  :: Position
  -> Maybe Velocity
  -> Maybe Accel
  -> Maybe Traction
  -> (Position, Maybe Velocity)
applyPhysics p v a t = (p', v')
  where
    v' = fmap (applyAccel (or0 a) . applyTraction (or0 t)) v
    p' = or0 v' `applyVelocity` p

applyVelocity :: Velocity -> Position -> Position
applyVelocity (Velocity v) (Position p) = Position (p + v)

applyAccel :: Accel -> Velocity -> Velocity
applyAccel (Accel a) (Velocity v) = Velocity (v + a)

applyTraction :: Traction -> Velocity -> Velocity
applyTraction (Traction t) (Velocity v) = Velocity v'
  where
    spd = norm v
    spd' = max 0 (spd - t)
    vdir = signorm v
    v' = vdir ^* spd'
