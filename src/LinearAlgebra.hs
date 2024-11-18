module LinearAlgebra
( KVectorSpace(..)
, KSpaceOperator(..)
, module Data.List -- transpose
) where

import Data.List (transpose)

class (Show k, Eq k) => KVectorSpace k where
  (^+^) :: [k] -> [k] -> [k]
  (.*^) :: k -> [k] -> [k]
  (^*.) :: [k] -> k -> [k]

  zeroV :: [k]

  negateV :: [k] -> [k]
  (^-^) :: [k] -> [k] -> [k]

  inverseK :: k -> k
  (^/.) :: [k] -> k -> [k]

  -- defaults
  (^-^) u w = u ^+^ (negateV w)
  (^*.) = flip (.*^)
  (^/.) u x = u ^*. (inverseK x)


instance KVectorSpace Float where
  (^+^) = zipWith (+)
  (.*^) x = map (x*)
  zeroV = repeat 0
  negateV = map ((-1) *)
  inverseK x = 1/x



class (KVectorSpace k) => KSpaceOperator k where
  (**.) :: [[k]] -> [k] -> [k]
  (.**.) :: [[k]] -> [[k]] -> [[k]]
  (.++.) :: [[k]] -> [[k]] -> [[k]]


instance KSpaceOperator Float where
  (**.) a x = map (\row -> sum $ zipWith (*) row x) a
  (.**.) a b = transpose $ map (\bCol -> a **. bCol) $ transpose b
  (.++.) a b = map (\(aRow,bRow) -> zipWith (+) aRow bRow) $ zip a b
