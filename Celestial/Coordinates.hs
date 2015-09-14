{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Coordinates for celestial sphere
module Celestial.Coordinates (
    -- * Spherical coordinates
    Spherical(..)
  , fromSpherical
    -- ** Coordinate systems
  , PhiDirection(..)
  , SphericalCoord(..)
  , HorizonalCoord
  , EquatorialCoord
  , J1900
  , J1950
  , J2000
  , Proj
    -- * Coordinate transformations
  , CoordTransform(..)
  , inverseTansform
  , toCoord
  , lookAtHorizontal
  , lookAtEquatorial
    -- * Other data types
  , GreatCircle(..)
    -- * Projection plane
  , ProjCoord(..)
  ) where

import Data.Angle
import Control.Category
import Data.Typeable
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

deriving instance (Show a, Unbox F.N3 a) => Show (Spherical c a)
deriving instance (Eq a,   Unbox F.N3 a) => Eq   (Spherical c a)


-- | Convert from spherical coordinates to unit vector representation
-- for equatorial coordinates. (RA grows CCW)
fromSpherical
  :: forall α δ a c. (AngularUnit α, AngularUnit δ, SphericalCoord c, Floating a, Unbox F.N3 a)
  => Angle α a
  -> Angle δ a
  -> Spherical c a
{-# INLINE fromSpherical #-}
fromSpherical α δ = Spherical $
  F.mk3 x y z
  where
    x = c * cos' α
    y = sign $ c * sin' α
    z = sin' δ
    c = cos' δ
    sign = case phiDirection ([] :: [c]) of
             CW  -> negate
             CCW -> id


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
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

-- | Inverse transform
inverseTansform
  :: (Floating a, Unbox F.N3 a, Unbox F.N4 a)
  => CoordTransform a c1 c2
  -> CoordTransform a c2 c1
inverseTansform (CoordTransform q) = CoordTransform $ recip q
{-# INLINE inverseTansform #-}

-- | Transform spherical coordinates from one coordinate system to
--   another.
toCoord
  :: (Unbox F.N3 a, Unbox F.N4 a, Floating a)
  => CoordTransform a c1 c2
  -> Spherical c1 a
  -> Spherical c2 a
toCoord (CoordTransform q) (Spherical v)
  = Spherical $ rotateVector q v
{-# INLINE toCoord #-}


-- | Great circle on celestial sphere. It's specified by axis of
--   rotation
newtype GreatCircle c a = GreatCircle (Spherical c a)


----------------------------------------------------------------
-- Transformations
----------------------------------------------------------------

-- | Simple camera coordinate transformation for
lookAtHorizontal
  :: (Unbox F.N3 a, Unbox F.N4 a, RealFloat a, AngularUnit t1, AngularUnit t2)
  => Angle t1 a                 -- ^ Azimuth
  -> Angle t2 a                 -- ^ Height
  -> CoordTransform a HorizonalCoord Proj
lookAtHorizontal a h = CoordTransform
  $ 1
  * rotZ (pi/2)
  * rotY (pi/2)
  * rotY (asRadians h)
  * rotZ (asRadians a)

-- | Simple camera coordinate transformation for
lookAtEquatorial
  :: (Unbox F.N3 a, Unbox F.N4 a, RealFloat a, AngularUnit t1, AngularUnit t2)
  => Angle t1 a                 -- ^ Right ascension
  -> Angle t2 a                 -- ^ Declination
  -> CoordTransform a (EquatorialCoord c) Proj
lookAtEquatorial α δ = CoordTransform
  $ 1
  * rotZ (pi/2)
  * rotY (pi/2)
  * rotY (asRadians δ)
  * rotZ (negate $ asRadians α)



----------------------------------------------------------------
-- Type tags for coordinate systems
----------------------------------------------------------------

-- | In which direction angle φ grows
data PhiDirection
  = CW                          -- ^ φ grows clockwise
  | CCW                         -- ^ φ grows counterclockwise

-- | In which direction angle φ (azimuth, RA) grows. Different
--   coordinate systems use different conventions.
class SphericalCoord c where
  phiDirection :: p c -> PhiDirection


-- | Horizontal coordinate system
data HorizonalCoord deriving Typeable

-- | Equatorial coordinate system. It's parametrized by epoch
data EquatorialCoord epoch deriving Typeable

data J1900 deriving Typeable
data J1950 deriving Typeable
data J2000 deriving Typeable


-- | Projection coordinate system.
data Proj

instance SphericalCoord HorizonalCoord where
  phiDirection _ = CW
instance SphericalCoord (EquatorialCoord c) where
  phiDirection _ = CCW



----------------------------------------------------------------
-- Projection plane
----------------------------------------------------------------

-- | Projection of celestial coordinates to 2D plane
newtype ProjCoord a = ProjCoord (Vec2 a)

type instance F.Dim ProjCoord = F.N2
instance Unbox F.N2 a => F.Vector ProjCoord a where
  inspect  (ProjCoord v) = F.inspect v
  construct = ProjCoord `fmap` F.construct
