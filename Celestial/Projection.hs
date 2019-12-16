{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Common interface for calculating projections from sphere to plane
module Celestial.Projection where

import Control.Monad (guard)
import Celestial.Coordinates
import qualified Data.Vector.Fixed as F



-- | Data type which describe projection from sphere to 2D plane. Note
--   that both projection and unprojection are partial functions
--   although either could be total for particular projection.
--
--   Point (0,0,-1) maps to (0,0)
data Projection a = Projection
  { project   :: Spherical Proj a -> Maybe (ProjCoord a)
    -- ^
  , unproject :: ProjCoord a      -> Maybe (Spherical Proj a)
    -- ^
  , maxR      :: Maybe Double
    -- ^ Maximum possible distance for projected point. Nothing is
    -- it's infinity
  }


-- | Orthographic projection
orthographic :: Projection Double
orthographic = Projection
  { project = \(Spherical v) -> case F.convert v of
      (x,y,z) | z < 0     -> Just $ ProjCoord $ F.mk2 x y
              | otherwise -> Nothing
  , unproject = \(F.convert -> (x,y)) -> case x*x + y*y of
      z2 | z2 >= 1   -> Nothing
         | otherwise -> Just $ Spherical $ F.mk3 x y (sqrt $ 1 - z2)
  , maxR      = Just 1
  }

-- | Stereographic projection
stereographic :: Projection Double
stereographic = Projection
  { project   = \(Spherical v) -> case F.convert v of
      (x,y,z) | z < 1     -> Just $ ProjCoord $ F.mk2 (x / (1-z)) (y / (1-z))
              | otherwise -> Nothing
  , unproject = \(F.convert -> (x,y)) ->
      let z2 = x*x + y*x
      in Just $ Spherical $ F.mk3 ( 2*x   / (1+z2))
                                  ( 2*y   / (1+z2))
                                  ((z2-1) / (1+z2))
  , maxR      = Nothing
  }

gnomonic :: Projection Double
gnomonic = Projection
  { project   = \(Spherical v) -> case F.convert v of
      (x,y,z) | z < 0     -> Just $ ProjCoord $ F.mk2 (x / (-z)) (y / (-z))
              | otherwise -> Nothing
  , unproject = \(F.convert -> (x,y)) ->
      let l = sqrt (1 + x*x + y*y)
      in Just $ Spherical $ F.mk3 (x/l) (y/l) (-1/l)
  , maxR      = Nothing
  }

-- | Azimuthal equidistant projection
azimuthalEquidistant :: Projection Double
azimuthalEquidistant = Projection
  { project = \(Spherical v) -> case F.convert v of
      (x,y,z) | z < 0     -> Just $ F.convert $ scaleTo (acos (negate z)) (x,y)
              | otherwise -> Nothing
  , unproject = \(F.convert -> (x,y)) -> do
       let θ = sqrt $ x*x + y*y
       guard $ θ < pi/2
       let sc = sinc θ
       case θ of
         0 -> return $ Spherical $ F.mk3 0 0 (-1)
         _ -> return $ Spherical $ F.mk3 (x*sc) (y*sc) (cos θ)
  , maxR      = Just (pi/2)
  }

-- | Lambert's azimuthal equal area projection
azimuthalEqualArea :: Projection Double
azimuthalEqualArea = Projection
  { project = \(Spherical v) -> case F.convert v of
      (x,y,z) ->
        let r  = 2 * sqrt ((1 + z) / 2) -- Radius of projection point
        in Just $ F.convert $ scaleTo r (x,y)
  , unproject = undefined
  , maxR = Just 2
  }

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Scale 2D point to given radius
scaleTo :: Double -> (Double,Double) -> (Double,Double)
scaleTo _ (0,0) = (0,0)
scaleTo r (x,y) = (x*s, y*s)
  where
    r0 = sqrt $ x*x + y*y
    s  = r/r0

-- | Safe sin(x)/x
sinc :: Double -> Double
sinc 0 = 1
sinc x = sin x / x
