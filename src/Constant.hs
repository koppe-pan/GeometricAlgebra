module Constant
  where

import Prelude hiding (Monoid, Semigroup, String, abs, filter, foldl, gcd, map, negate, pure, (+), (-), (*), (/))
import qualified Prelude ((+), (-), (*), (/), gcd)
import Data.String
import Data.List( (\\), intersect, nubBy )
import qualified Data.List as List (foldl)
import Data.Map (Map, assocs, elems, filter, fromList, fromListWith, keys, map, mapKeys, unionWith)

type Term = [String]
type Numerator = Map Term Int
type Denominator = Map Term Int
type Constant = (Numerator, Denominator)

constant :: Numerator -> Denominator -> Constant
constant a b = (g$f a, g$f b)
  where
    dk = gcd . keys $ a <> b
    dv = gcdd . elems $ a <> b
    f = mapKeys $ \l -> l \\ dk
    g = map $ \l -> l `div` dv
    gcdd [] = 1
    gcdd (a':as) = List.foldl Prelude.gcd a' as

gcd :: [Term] -> Term
gcd [] = []
gcd (a:as) = List.foldl intersect a as

x = ["x", "z", "c", ""] :: Term
y = ["y", "w", "c", ""] :: Term
z = ["z", "c", ""] :: Term
w = ["w", "c", ""] :: Term
numerator = fromList [(x, 2), (y, 4)] :: Numerator
denominator = fromList [(z, 6), (w, 2)] :: Denominator

(*) :: Constant -> Constant -> Constant
(na, da) * (nb, db) = constant (f na nb) $ f da db
  where
    f a b = filter (/= 0) $ fromListWith (Prelude.+) $ basisMul <$> assocs a <*> assocs b

    basisMul (s, m) (t, n) = gnome [] (s ++ t) (m Prelude.* n) where
      gnome pre (a:b:rest) c

        -- Vectors are in order: move on.
        | a <= b  = gnome (pre ++ [a]) (b:rest) c

        -- Wrong order: flip and back up one step. Since the vectors are
        -- orthogonal, we flip the sign of the geometric product.
        | a > b  = back pre (b:a:rest) c

        where
          back []   rest' c' = gnome []         rest'             c'
          back pre' rest' c' = gnome (init pre) (last pre':rest') c'

      gnome pre rest c = (pre ++ rest :: Term, c)
