module Prueba () where

import Control.Monad

data Otro a = Otro {fromOtro :: ((a -> Beta) -> Beta)}

instance Monad Otro where
  return x       = Otro $ \k -> k x
  (Otro f) >>= g = Otro $
                   \k -> fromOtro (Otro f) (\x -> fromOtro (g x) k)

data Beta = Chamba (IO Beta)
          | Convive Beta Beta
          | Quieto    
          
main = undefined               
