{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
-- |
-- Coordinates for celestial sphere
module Celestial.Coordinates where

import Data.Angle
import qualified Data.Vector.Fixed as F
import           Data.Vector.Fixed.Unboxed (Vec2,Vec3,Vec,Unbox)
import Data.Quaternion (Quaternion)


----------------------------------------------------------------
-- Coordinates
----------------------------------------------------------------

-- | Coordinate on celestial sphere. They're tagged by coordinate
--   system (horizontal, equatorial, etc). Coordinates are represented
--   as 3D vector with norm 1. This representation is redundant but
--   allows cheap conversion between different coordinate systems.
newtype Spherical c a = Spherical (Vec3 a)


-- | Convert from spherical coordinates (coordinates are 
fromSperical
  :: (AngularUnit α, AngularUnit δ, Floating a, Unbox F.N3 a)
  => Angle α a -> Angle δ a -> Spherical c a
fromSperical α δ = Spherical $
  F.mk3 (f * cos' α) (f * sin' α) z
  where
    z = sin' δ
    f = cos' δ

-- | Projection of celestial coordinates to 2D plane
newtype ProjCoord a = ProjCoord (Vec2 a)

type instance F.Dim ProjCoord = F.N2
instance Unbox F.N2 a => F.Vector ProjCoord a where
  inspect  (ProjCoord v) = F.inspect v
  construct = ProjCoord `fmap` F.construct


-- | Great circle on celestial sphere. It's specified by axis of
--   rotation
newtype GreatCircle c a = GreatCircle (Spherical c a)


----------------------------------------------------------------
-- Type tags for coordinate systems
----------------------------------------------------------------

-- | Horizontal coordinate system
data HorizonalCoord

-- | Equatorial coordinate system. It's parametrized by epoch
data EquatorialCoord epoch

data J1900
data J1950
data J2000
