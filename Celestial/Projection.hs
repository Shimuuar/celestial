{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Common interface for calculating projections from sphere to plane
module Celestial.Projection where


import Celestial.Coordinates
import qualified Data.Vector.Fixed as F



-- | Data type which describe projection from sphere to 2D plane. Note
--   that both projection and unprojection are partial functions
--   although either could be total for particular projection.
--
--   Point (0,0,-1) maps to (0,0)
data Projection c a = Projection
  { project   :: Spherical Proj a -> Maybe (ProjCoord a)
    -- ^ 
  , unproject :: ProjCoord a      -> Maybe (Spherical Proj a)
    -- ^
  , maxR      :: Maybe Double
    -- ^ Maximum possible distance for projected point
  }


-- | Orthographic projection
orthographic :: Projection c Double
orthographic = Projection
  { project = \(Spherical v) -> case F.convert v of
      (x,y,z) | z < 0     -> Just $ ProjCoord $ F.mk2 x y
              | otherwise -> Nothing
  , unproject = \(F.convert -> (x,y)) -> case x*x + y*y of
      z2 | z2 >= 1   -> Nothing
         | otherwise -> Just $ Spherical $ F.mk3 x y (sqrt $ 1 - z2)
  , maxR      = Just 1
  }


-- Projections to be supported:
--   + Lambert-azimuthal equal area
--   + Azimuthal equidistant
--   + Orthographic
--   + Equirectangular
--   + Stereographic
--   + Glonomonic
