{-# LANGUAGE TypeFamilies #-}
-- |
-- Coordinates for celestial sphere
module Celestial.Coordinates where

import qualified Data.Vector.Fixed as F
import           Data.Vector.Fixed.Unboxed (Vec2,Vec3)
import Data.Quaternion (Quaternion)


----------------------------------------------------------------
-- Coordinates
----------------------------------------------------------------

-- | Coordinate on celestial sphere. They're tagged by coordinate
--   system (horizontal, equatorial, etc). Coordinates are represented
--   as 3D vector with norm 1. This representation is redundant but
--   allows cheap conversion between different coordinate systems.
newtype Spherical c a = Spherical (Vec3 a)

-- | Projection of celestial coordinates to 2D plane
newtype ProjCoord a = ProjCoord (Vec2 a)



----------------------------------------------------------------
-- Type tags for coordinate systems
----------------------------------------------------------------

-- | Horizontal coordinate system
data HorizonalCoord

-- | Equatorial coordinate system. It's parametrized by epoch
data EquatorialCoord epoch
