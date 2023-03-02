{-# LANGUAGE ExplicitForAll #-}

module Yoneda where

imager :: forall r . ((Bool -> r) -> [r])
imager iffie = fmap iffie [True, False, True, True]

data Color = Red | Green | Blue        deriving Show
data Note  = C | D | E | F | G | A | B deriving Show

colorMap x = if x then Blue else Red
heatMap  x = if x then 32   else 212
soundMap x = if x then C    else G
idBool :: Bool -> Bool
idBool x = x
