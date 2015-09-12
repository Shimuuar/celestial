{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Function for working with time
--
-- The Universal Time (UT) is based on rotation of Earth and since
-- Earth's rotation is slowing down it's not regular time scale.
--
-- FIXME: At the moment we don't introduce correction for that.
--
-- FIXME: UT1!
module Celestial.Time (
    -- * Juliad day
    JD(..)
  , normalizeJD
  , currentJD
    -- * Sidereal time
  , GST(..)
  , meanGST
  , LST(..)
  , meanLST
  ) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Data    (Typeable,Data)
import GHC.Generics (Generic)

import Data.Angle
import Celestial.Geo



----------------------------------------------------------------
-- Julian date
----------------------------------------------------------------

-- | Julian day
--
--   Double doesn't give enough precision since JD is too big. So we
--   use Integer to hold number of days and Double to hold fractional
--   part.
data JD = JD !Integer !Double
          deriving (Show,Eq, Typeable,Data,Generic)

instance Num JD where
  fromInteger i = JD i 0
  JD n1 t1 + JD n2 t2 = normalizeJD $ JD (n1+n2) (t1+t2)
  JD n1 t1 - JD n2 t2 = normalizeJD $ JD (n1-n2) (t1-t2)
  (*)    = error "No sensible * for JD"
  abs    = error "No sensible abs for JD"
  signum = error "No sensible signum for JD"


-- | Obtain current JD
currentJD :: IO JD
currentJD = do
  UTCTime { utctDay     = ModifiedJulianDay day
          , utctDayTime = dt
          } <- getCurrentTime
  return $ case (realToFrac dt / 86400) + 0.5 of
    x | x >= 1    -> JD (day + 2400001) (x - 1)
      | otherwise -> JD (day + 2400000)  x

-- | Normalize JD. Put fractional part in [0,1) range
normalizeJD :: JD -> JD
normalizeJD x@(JD n dt)
  | dt < 0    = normalizeJD $ JD (n-1) (dt+1)
  | dt >= 1   = normalizeJD $ JD (n+1) (dt-1)
  | otherwise = x


----------------------------------------------------------------
-- Sidereal time
----------------------------------------------------------------

-- | Greenwich sidereal time in hours
newtype GST = GST Double
            deriving (Show,Eq,Ord, Typeable,Data,Generic)

-- | Calculate mean sidereal time at Greenwich (0th meridian)
meanGST :: JD -> GST
-- FIXME: formulae taken from Meeus (Ch.11) Find source
meanGST jd = case normalizeJD jd of
  JD n dayT -> GST h
    where
      t = (fromIntegral (n - 2451545) + 0.5) / 36525
      θ = 6*3600 + 41*60 + 50.54841
        + (8640184.812866 * t)
        + (0.093104       * t*t)
        - (0.0000062      * t*t*t)
      -- Calculate hours
      (_::Int,dt) = properFraction (θ / 86400)
      h = dt * 24 + (dayT-0.5) * 24 * 1.00273790935


-- | Local sidereal time in hours
newtype LST = LST Double
            deriving (Show,Eq,Ord, Typeable,Data,Generic)

-- | Calculate mean sidereal time at location
meanLST :: Location -> JD -> LST
meanLST loc jd
  = LST (t + d)
  where
    GST   t = meanGST jd
    Angle d = convertAngle (geoLongitude loc) :: Angle HourRA Double
