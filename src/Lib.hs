{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import GHC.Generics
import Data.Proxy

data MyMaybe a = MyNothing | MyJust a
  deriving (Generic)


data MyTriple = Blah | Blah2 | Blah3 | Blah4 | Blah5 
  deriving stock (Generic)


data MySingle = Single deriving Generic

data MyMediumOne = A1 Int Int Int Bool
  deriving stock Generic
  deriving anyclass Size

data MyBigOne = B1 Int Int MyMediumOne | B2 Int Bool 
  deriving stock (Generic)
  deriving anyclass Size

instance Size MyTriple
instance Size MySingle


  

class Size a where 
  numberOfConstructors :: Proxy a -> Int
  default numberOfConstructors :: GSize (Rep a) => Proxy a -> Int
  numberOfConstructors _ = gnumberOfConstructors (Proxy :: Proxy (Rep a))

  maxSize :: Proxy a -> Int
  default maxSize :: GSize (Rep a) => Proxy a -> Int
  maxSize _ = gmaxSize (Proxy :: Proxy (Rep a))


-- Define the GSize type class and its instances
class GSize (f :: k) where
  gnumberOfConstructors :: Proxy f -> Int
  gmaxSize :: Proxy f -> Int


instance (GSize f) => GSize (M1 C c f) where 
  gnumberOfConstructors _ = 1
  gmaxSize _ = gmaxSize (Proxy :: Proxy f)

instance GSize f => GSize (M1 i c f) where 
  gnumberOfConstructors _ = gnumberOfConstructors (Proxy :: Proxy f)
  gmaxSize _ = gmaxSize (Proxy :: Proxy f)

instance (GSize a, GSize b) => GSize (a :*: b) where 
  gnumberOfConstructors _ = 0
  gmaxSize _ = gmaxSize (Proxy :: Proxy a) + gmaxSize (Proxy :: Proxy b)

instance (GSize a, GSize b) => GSize (a :+: b) where 
  gnumberOfConstructors _ = gnumberOfConstructors (Proxy :: Proxy a) + gnumberOfConstructors (Proxy :: Proxy b)
  gmaxSize _ = max (gmaxSize (Proxy :: Proxy a)) (gmaxSize (Proxy :: Proxy b))

instance GSize U1 where 
  gnumberOfConstructors _ = 0
  gmaxSize _ = 0

instance (Size a) => GSize (K1 i a) where 
  gnumberOfConstructors _ = numberOfConstructors (Proxy :: Proxy a) 
  gmaxSize _ = maxSize (Proxy :: Proxy a)

instance (Size a) => Size (MyMaybe a)


instance Size Int where 
  numberOfConstructors _ = 0
  maxSize _ = 32


instance Size Bool where 
  numberOfConstructors _ = 0 
  maxSize _ = 1








