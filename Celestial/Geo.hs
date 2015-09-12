{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- |
-- Module for working with points on the Earth
module Celestial.Geo (
    Location(..)
  ) where

import Data.Angle
import Data.Data    (Typeable)
import GHC.Generics (Generic)



-- | Location on Earth
data Location = Location
  { geoLatitude  :: Angle Degrees Double
  , getLongitude :: Angle Degrees Double
  }
  deriving (Show,Eq, Typeable,Generic)
