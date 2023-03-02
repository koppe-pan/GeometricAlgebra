module ElectroMagnetism where

import GeometricAlgebra

e0 :: Basis
e0 = Basis "e0" (-1)

e1 :: Basis
e1 = Basis "e1" 1

e2 :: Basis
e2 = Basis "e2" 1

e3 :: Basis
e3 = Basis "e3" 1

env :: [Basis]
env = [e0,e1,e2,e3]
