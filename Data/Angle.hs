{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Type-safe angles
module Data.Angle where

import Data.Data     (Data,Typeable,typeOf,Proxy(..))
import GHC.Generics  (Generic)

-- | Type safe wrapper for angle
newtype Angle t a = Angle a
                    deriving (Eq,Ord, Data,Typeable,Generic)

instance (Typeable t, AngularUnit t, Floating a, Show a) => Show (Angle t a) where
  show (Angle a) = "Angle "
                ++ show (typeOf (undefined :: t))
                ++ " "
                ++ show (a * angularUnit (Proxy :: Proxy t))

angle :: forall t a. (AngularUnit t, Floating a) => a -> Angle t a
angle a = Angle $ a / angularUnit ([] :: [t])

convertAngle
  :: forall a t1 t2. (Floating a, AngularUnit t1, AngularUnit t2)
  => Angle t1 a -> Angle t2 a 
convertAngle (Angle a) = Angle a

asRadians
  :: forall a t. (Floating a, AngularUnit t)
  => Angle t a -> a
asRadians (Angle a) = a

sin' :: (Floating a, AngularUnit t) => Angle t a -> a
sin' = sin . asRadians

cos' :: (Floating a, AngularUnit t) => Angle t a -> a
cos' = cos . asRadians

tan' :: (Floating a, AngularUnit t) => Angle t a -> a
tan' = tan . asRadians

asin' :: (Floating a, AngularUnit t) => a -> Angle t a
asin' = Angle . asin

acos' :: (Floating a, AngularUnit t) => a -> Angle t a
acos' = Angle . acos

atan' :: (Floating a, AngularUnit t) => a -> Angle t a
atan' = Angle . atan


data Radians  deriving (Typeable)
data Degrees  deriving (Typeable)
data Minutes  deriving (Typeable)
data Seconds  deriving (Typeable)
data HourRA   deriving (Typeable)
data MinuteRA deriving (Typeable)
data SecondRA deriving (Typeable)

-- | Type class for type-safe angles
class AngularUnit t where
  -- | Value of 1rad in given units
  angularUnit :: Floating a => p t -> a

instance AngularUnit Radians  where angularUnit _ = 1
instance AngularUnit Degrees  where angularUnit _ = 180 / pi 
instance AngularUnit Minutes  where angularUnit _ = 60 * 180 / pi
instance AngularUnit Seconds  where angularUnit _ = 60 * 60 * 180 / pi
instance AngularUnit HourRA   where angularUnit _ = 12 / pi
instance AngularUnit MinuteRA where angularUnit _ = 60 * 12 / pi
instance AngularUnit SecondRA where angularUnit _ = 60 * 60 * 12 / pi
