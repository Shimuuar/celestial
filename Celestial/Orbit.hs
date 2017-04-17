{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
module Celestial.Orbit (
    PlanarElements(..)
  , trueAnomalyAtT
  , coordinatesAtT
  ) where

import Data.Classes.AdditiveGroup
import Data.Angle
import qualified Data.Vector.Fixed as F


-- | Orbital elements which specify position of body in the orbital
--   plane
--
--   2D coordinate system uses following conventions. Central body
--   located in beginning of coordinates. X axis points to orbit
--   pericenter. Object moves in the plane in counterclockwise
--   direction.
data PlanarElements t = PlanarElements
  { eccentricity  :: !Double           -- ^ Eccentricity of an orbit
  , semimajorAxis :: !Double           -- ^ Major semiaxis of an orbit
  , period        :: !Double           -- ^ Period of orbiting body
  , meanAnomaly   :: !(Angle t Double) -- ^ Mean anomaly
  , epoch         :: !Double           -- ^ Epoch at which mean anomaly is measured
  }
  deriving (Show)


-- | Calculate true anomaly at given time.
trueAnomalyAtT :: PlanarElements t -- ^ Orbital elements
               -> Double           -- ^ Time
               -> Angle t Double
trueAnomalyAtT PlanarElements{..} t
  | ecc > pi  = Angle (2*pi) .-. θ
  | otherwise = θ
  where
    -- Mean anomaly at time T
    Angle m0 = meanAnomaly
    m        = reduceAngle $ m0 + 2 * pi * (t - epoch) / period
    -- Calculate eccentric anomaly
    --
    -- FIXME: Iteration is not terribly efficient especially for high
    --        eccentricities but will do for now
    ecc  = solveByIteration (\ecc0 -> m + eccentricity * sin ecc0) m
    cosE = cos ecc
    -- True anomaly. We need to move angle into full [0,2π] range
    θ    = acos' $ (cosE - eccentricity)
                 / (1 - eccentricity * cosE)

-- | 2D coordinates at given time
coordinatesAtT :: (F.Dim v ~ F.N2, F.Vector v Double)
               => PlanarElements t -> Double -> v Double
coordinatesAtT elts@PlanarElements{..} t
  = F.mk2 (r * cos' θ) (r * sin' θ)
  where
    θ = trueAnomalyAtT elts t
    e = eccentricity
    r = semimajorAxis * (1 - e*e)
                      / (1 + e * cos' θ)

-- Find fixed point of function
solveByIteration :: Eq a => (a -> a) -> a -> a
solveByIteration f
  = loop
  where
    loop !x | x' == x   = x
            | otherwise = loop x'
            where
              x' = f x

-- Reduce angle in radians to [0,2π] range
reduceAngle :: Double -> Double
reduceAngle x = case properFraction (x/(2*pi)) of
  (_::Int,f) | f == 0 || x >= 0 -> 2*pi*f
             | otherwise        -> 2*pi*(f + 1)
