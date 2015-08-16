-- |
-- Common interface for calculating projections from sphere to plane
module Celestial.Projection where


import Celestial.Coordinates

-- | Data type which describe projection from sphere to 2D plane. Note
--   that both projection and unprojection are partial functions
--   although either could be total for particular projection.
data Projection c a = Projection
  { project   :: Spherical c a -> Maybe (ProjCoord a)
  , unproject :: ProjCoord a   -> Maybe (Spherical c a)
  }

