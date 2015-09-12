{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
-- |
-- Coordinates for celestial sphere
module Celestial.Coordinates (
    -- * Spherical coordinates
    Spherical(..)
  , fromSperical
  , CoordTransform(..)
  , toCoord
    -- ** Coordinate systems
  , HorizonalCoord
  , EquatorialCoord
  , J1900
  , J1950
  , J2000
  , Proj
    -- ** Other data types
  , GreatCircle(..)
    -- * Projection plane
  , ProjCoord(..)
  ) where

import Data.Angle
import Control.Category
import qualified Data.Vector.Fixed as F
import           Data.Vector.Fixed.Unboxed (Vec2,Vec3,Unbox)
import Data.Quaternion -- (Quaternion)

import Prelude hiding ((.),id)


----------------------------------------------------------------
-- Spherical coordinates
----------------------------------------------------------------

-- | Coordinate on celestial sphere. They're tagged by coordinate
--   system (horizontal, equatorial, etc). Coordinates are represented
--   as 3D vector with norm 1. This representation is redundant but
--   allows cheap conversion between different coordinate systems.
newtype Spherical c a = Spherical (Vec3 a)


-- | Convert from spherical coordinates to unit vector representation
fromSperical
  :: (AngularUnit α, AngularUnit δ, Floating a, Unbox F.N3 a)
  => Angle α a -> Angle δ a -> Spherical c a
fromSperical α δ = Spherical $
  F.mk3 (f * cos' α) (f * sin' α) z
  where
    z = sin' δ
    f = cos' δ

-- | Coordinate transformation from coordinate system @c1@ to
-- coordinate system @c2@.
newtype CoordTransform a c1 c2 = CoordTransform
  { coordTransformRepr :: Quaternion a }

deriving instance (Show a, Unbox F.N4 a) => Show (CoordTransform a c1 c2)
deriving instance (Eq a,   Unbox F.N4 a) => Eq   (CoordTransform a c1 c2)

instance (Unbox F.N4 a, Floating a) => Category (CoordTransform a) where
  id = CoordTransform 1
  -- FIXME: is composition correct?
  CoordTransform f . CoordTransform g = CoordTransform (f * g)


-- | Transform spherical coordinates from one coordinate system to
--   another.
toCoord
  :: (Unbox F.N3 a, Unbox F.N4 a, Floating a)
  => CoordTransform a c1 c2
  -> Spherical c1 a
  -> Spherical c2 a
toCoord (CoordTransform q) (Spherical v)
  = Spherical $ rotateVector q v

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


-- | Projection coordinate system. 
data Proj



----------------------------------------------------------------
-- Projection plane
----------------------------------------------------------------

-- | Projection of celestial coordinates to 2D plane
newtype ProjCoord a = ProjCoord (Vec2 a)

type instance F.Dim ProjCoord = F.N2
instance Unbox F.N2 a => F.Vector ProjCoord a where
  inspect  (ProjCoord v) = F.inspect v
  construct = ProjCoord `fmap` F.construct
