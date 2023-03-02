{-# LANGUAGE FlexibleInstances #-}

module GeometricAlgebra
  ( Basis(..)
  , Blade
  , Multivector
  , GeometricAlgebra
  , GeometricAlgebraState
  , apply
  , geometricAlgebra
  , geometricAlgebraWithBasis
  , scalar
  , setBasis
  , pseudoScalar
  , negate
  , (+)
  , (-)
  , (*)
  , (^)
  )
  where

import Prelude hiding (Monoid, Semigroup, String, abs, filter, negate, pure, (+), (-), (*), (^))
import qualified Prelude ((+), (*))
import Data.Monoid (Monoid)
import Data.Semigroup (Semigroup)
import Data.String
import Data.List (sort, union)
import Data.Map (Map, assocs, filter, fromListWith, singleton, unionWith)
import Control.Monad.State

data Basis = Basis {str :: String, square :: Double }
  deriving (Eq, Ord)

instance Show Basis where
  show = str

newtype Blade = Blade [Basis]
  deriving Eq

instance Ord Blade where
  Blade a <= Blade b = if length a == length b then a <= b
                                               else length a <= length b

instance Show Blade where
  show (Blade a) = foldl (<>) "" $ fmap str a

instance Semigroup Blade where
  Blade a <> Blade b = Blade $ a <> b

instance Monoid Blade where
  mempty = Blade [Basis "" 1]

type Multivector = Map Blade Double

instance {-# OVERLAPPING #-} Monoid Multivector where
  mempty = singleton mempty 1

type GeometricAlgebra = ([Basis], Multivector)

instance {-# OVERLAPPING #-} Show GeometricAlgebra where
  show (basis', map') = foldl f (show basis' <> "\n") (assocs map')
    where
      f z (value, coeff) = z <> " + " <> show coeff <> show value

(+) :: GeometricAlgebra -> GeometricAlgebra -> GeometricAlgebra
(aBasis, aMap) + (bBasis, bMap) = (aBasis `union` bBasis, filter (/= 0) $ unionWith ((Prelude.+) :: Double -> Double -> Double) aMap bMap)

(-) :: GeometricAlgebra -> GeometricAlgebra -> GeometricAlgebra
a - b = a + negate b

negate :: GeometricAlgebra -> GeometricAlgebra
negate = (<>) (scalar (-1))

(*) :: GeometricAlgebra -> GeometricAlgebra -> GeometricAlgebra
a * b = scalar (1/2) <> ((a <> b) + (b <> a))

(^) :: GeometricAlgebra -> GeometricAlgebra -> GeometricAlgebra
a ^ b = scalar (1/2) <> ((a <> b) - (b <> a))

instance {-# OVERLAPPING #-} Monoid GeometricAlgebra where
  mempty = scalar 1

instance {-# OVERLAPPING #-} Semigroup GeometricAlgebra where
  (aBasis, aMap) <> (bBasis, bMap) = (sort $ aBasis `union` bBasis, mul aMap bMap)
      where
        mul a b = filter (/= 0) $ fromListWith (Prelude.+) $ basisMul <$> assocs a <*> assocs b

        basisMul (Blade s, m) (Blade t, n) = gnome [] (s ++ t) (m Prelude.* n) where
          gnome pre (a:b:rest) c

            -- Vectors are in order: move on.
            | a < b  = gnome (pre ++ [a]) (b:rest) c

            -- Same vector: remove both copies since magnitude = 1.
            | a == b = back pre rest (c Prelude.* square a)

            -- Wrong order: flip and back up one step. Since the vectors are
            -- orthogonal, we flip the sign of the geometric product.
            | a > b  = back pre (b:a:rest) (-c)

            where
              back []   rest' c' = gnome []         rest'             c'
              back pre' rest' c' = gnome (init pre) (last pre':rest') c'

          gnome pre rest c = (Blade $ pre ++ rest, c)

type GeometricAlgebraState = State GeometricAlgebra

apply :: (GeometricAlgebra -> GeometricAlgebra) -> GeometricAlgebraState GeometricAlgebra
apply f = state (\s -> let s' = f s in (val s', s'))
  where
    val = id

x :: Basis
x = Basis "x" 1
y :: Basis
y = Basis "y" 1
xx :: GeometricAlgebra
xx = geometricAlgebra (Blade [x]) 2
yy :: GeometricAlgebra
yy = geometricAlgebra (Blade [y]) 3
xy :: GeometricAlgebra
xy = geometricAlgebra (Blade [x,y]) 5

hoge :: GeometricAlgebra
hoge = evalState stateMonad (scalar 1)
  where
    stateMonad = do
      _ <- apply (+ yy)
      _ <- apply (<> xy)
      pseudoScalar

geometricAlgebra :: Blade -> Double -> GeometricAlgebra
geometricAlgebra (Blade blade) coeff = (blade, singleton (Blade blade) coeff)

geometricAlgebraWithBasis :: [Basis] -> Blade -> Double -> GeometricAlgebra
geometricAlgebraWithBasis basis blade coeff = setBasis basis <> geometricAlgebra blade coeff

setBasis :: [Basis] -> GeometricAlgebra
setBasis basis = (basis, mempty)

scalar :: Double -> GeometricAlgebra
scalar = geometricAlgebra mempty

pseudoScalar :: GeometricAlgebraState GeometricAlgebra
pseudoScalar = do
  (basis, _) <- get
  return $ geometricAlgebra (Blade basis) 1
