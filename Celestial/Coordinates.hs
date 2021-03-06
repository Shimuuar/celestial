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
{-# LANGUAGE ViewPatterns               #-}
-- |
-- Data types for coordinates on celestial sphere and conversions
-- between different representations. Usually point on celestial
-- sphere are represented using as pair of angles. This representation
-- is compact conversions between different coordinate systems are
-- complicated and computationally costly so library uses another
-- representation for point on celestial sphere: vector with unit norm.
module Celestial.Coordinates (
    -- * Spherical coordinates
    Spherical(..)
    -- ** Coordinate systems
  , HorizonalCoord
  , EquatorialCoord
  , B1900
  , B1950
  , J2000
  , Proj
    -- ** Conversion to spherical coordinates
  , SphericalCoord(..)
  , PhiDirection(..)
  , fromSpherical
  , toSpherical
    -- * Coordinate transformations
  , CoordTransform(..)
  , inverseTansform
  , toCoord
    -- ** Concrete transformations
  , lookAt
  , equatorialToHorizontal
  , horizontalToEquatorial
    -- * Other data types
  , GreatCircle(..)
    -- * Projection plane
  , ProjCoord(..)
  ) where

import Control.Category
import Data.Typeable
import qualified Data.Vector.Fixed as F
import           Data.Vector.Fixed.Unboxed (Vec2,Vec3,Unbox)
import Data.Quaternion -- (Quaternion)

import Data.Angle
import Celestial.Geo  (Location(..))
import Celestial.Time (LST(..))

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



----------------------------------------------------------------
-- Type tags for coordinate systems
----------------------------------------------------------------

-- | Horizontal coordinate system
data HorizonalCoord deriving Typeable

-- | Equatorial coordinate system. It's parametrized by epoch
data EquatorialCoord epoch deriving Typeable

data B1900 deriving Typeable
data B1950 deriving Typeable
data J2000 deriving Typeable


-- | Projection coordinate system. It projection coordinate system
-- point (0,0,-1) corresponds to center of field of view, Y point up
-- and X points right in projection plane.
data Proj



----------------------------------------------------------------
-- Conversions to/from spherical coords
----------------------------------------------------------------

-- | Convert from spherical coordinates to unit vector representation
fromSpherical
  :: forall α δ a c. (AngularUnit α, AngularUnit δ, SphericalCoord c, Floating a, Unbox F.N3 a)
  => Angle α a                  -- ^ Azimuth\/RA\/etc [0,2π]
  -> Angle δ a                  -- ^ Height\/declination\/etc [-π\/2,π\/2]
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

-- | Convert from spherical coordinates to unit vector representation
toSpherical
  :: forall α δ a c. (AngularUnit α, AngularUnit δ, SphericalCoord c, Floating a, Ord a, Unbox F.N3 a)
  => Spherical c a              -- ^ Point on celestial sphere
  -> (Angle α a, Angle δ a)     -- ^ Pair of Azimuth\/RA\/etc [0,2π]
                                -- and height\/declination\/etc [-π\/2,π\/2]
{-# INLINE toSpherical #-}
toSpherical (Spherical (F.convert -> (x,y,z))) =
  ( case phiDirection ([] :: [c]) of
      CW  -> Angle aCW
      CCW -> Angle aCCW
  , asin' z
  )
  where
    aCCW = case atan (y/x) of
             a | x > 0 && y >= 0 -> a
               | x > 0 && y <  0 -> a + 2*pi
               | otherwise       -> a + pi
    aCW  = case atan (y/x) of
             a | x > 0 && y >= 0 -> 2*pi - a
               | x > 0 && y <  0 -> -a
               | otherwise       -> pi - a


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



-- | Convert equatorial coordinates to horizontal
equatorialToHorizontal
  :: Location
  -> LST
  -> CoordTransform Double (EquatorialCoord j) HorizonalCoord
equatorialToHorizontal loc (LST lst) = CoordTransform
  $ rotation (negate $ asRadians a) axis
  * rotY (negate $ pi/2 - asRadians (geoLatitude loc))
  where
    a    = angle lst :: Angle HourRA Double
    axis = (- (cos' (geoLatitude loc))
           , 0
           , sin'   (geoLatitude loc)
           )

-- | Convert horizontal coordinates to equatorial
horizontalToEquatorial
  :: Location
  -> LST
  -> CoordTransform Double HorizonalCoord (EquatorialCoord j)
horizontalToEquatorial loc lst
  = inverseTansform $ equatorialToHorizontal loc lst


-- | Great circle on celestial sphere. It's specified by axis of
--   rotation
newtype GreatCircle c a = GreatCircle (Spherical c a)


----------------------------------------------------------------
-- Transformations
----------------------------------------------------------------

-- | Simple camera coordinate transformation for
lookAt
  :: forall t1 t2 c a. (Unbox F.N3 a, Unbox F.N4 a, RealFloat a, AngularUnit t1, AngularUnit t2, SphericalCoord c)
  => Angle t1 a                 -- ^ Azimuth
  -> Angle t2 a                 -- ^ Height
  -> CoordTransform a c Proj
lookAt a h = CoordTransform
  $ 1
  * rotZ (pi/2)
  * rotY (pi/2)
  * rotY (asRadians h)
  * rotZ φ
  where
    φ = case phiDirection ([] :: [c]) of
          CW  -> asRadians a
          CCW -> negate $ asRadians a


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

-- | Shift and uniform scale scale transformation
data ShiftAndScale a = ShiftAndScale !a !(Vec2 a)

invertShiftAndScale :: (Unbox F.N2 a) => ShiftAndScale a -> ShiftAndScale a
invertShiftAndScale (ShiftAndScale s p0) =
  undefined

instance Unbox F.N2 a => Monoid (ShiftAndScale a) where
  mempty = ShiftAndScale 1 (F.mk2 0 0)
  ShiftAndScale s1 p1 `mappend` ShiftAndScale s2 p2
    = ShiftAndScale (s1 * s2) 
