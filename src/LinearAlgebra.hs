module LinearAlgebra
( KVectorSpace(..)
, KSpaceOperator(..)

, rotationMatrix
, rotationXMatrix
, rotationYMatrix
, rotationZMatrix
, lookatMatrix
, perspectiveMatrix
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

  norm :: [k] -> k
  normalize :: [k] -> [k]

  -- defaults
  (^-^) u w = u ^+^ (negateV w)
  (^*.) = flip (.*^)
  (^/.) u x = u ^*. (inverseK x)
  normalize v = v ^/. (norm v)


instance KVectorSpace Float where
  (^+^) = zipWith (+)
  (.*^) x = map (x*)
  zeroV = repeat 0
  negateV = map ((-1) *)
  inverseK x = 1/x
  norm v = sum $ map (**2) v



class (KVectorSpace k) => KSpaceOperator k where
  (**.) :: [[k]] -> [k] -> [k]
  (.**.) :: [[k]] -> [[k]] -> [[k]]
  (.++.) :: [[k]] -> [[k]] -> [[k]]


instance KSpaceOperator Float where
  (**.) a x = map (\row -> sum $ zipWith (*) row x) a
  (.**.) a b = transpose $ map (\bCol -> a **. bCol) $ transpose b
  (.++.) a b = map (\(aRow,bRow) -> zipWith (+) aRow bRow) $ zip a b


{- Useful Transformations -}

rotationMatrix :: [Float] -> [[Float]]
rotationMatrix rot =
  [ [ ux**2 * (1 - cos t) + cos t,     ux*uy * (1 - cos t) - uz*sin t,   ux*uz * (1 - cos t) + uy*sin t,  0]
  , [ ux*uy * (1 - cos t) + uz*sin t,  uy**2 * (1 - cos t) + cos t,      uy*uz * (1 - cos t) - ux*sin t,  0]
  , [ ux*uz * (1 - cos t) - uy*sin t,  uy*uz * (1 - cos t) + ux*sin t,   uz**2 * (1 - cos t) + cos t,     0]
  , [ 0,                               0,                                0,                               1]
  ] :: [[Float]]
  where t = norm rot
        u = normalize rot
        ux = u !! 0
        uy = u !! 1
        uz = u !! 2

rotationXMatrix :: Float -> [[Float]]
rotationXMatrix angle = rotationMatrix $ angle .*^ [1,0,0]

rotationYMatrix :: Float -> [[Float]]
rotationYMatrix angle = rotationMatrix $ angle .*^ [0,1,0]

rotationZMatrix :: Float -> [[Float]]
rotationZMatrix angle = rotationMatrix $ angle .*^ [0,0,1]


lookatMatrix :: [Float] -> [Float] -> [Float] -> [Float] -> [[Float]]
lookatMatrix p r u d = lookatA .**. lookatB
  where lookatA = [ r ++ [0]
                  , u ++ [0]
                  , d ++ [0]
                  , [0,0,0,1] ] :: [[Float]]
        lookatB = [ [1, 0, 0, -(p !! 0)]
                  , [0, 1, 0, -(p !! 1)]
                  , [0, 0, 1, -(p !! 2)]
                  , [0, 0, 0,      1   ] ] :: [[Float]]

perspectiveMatrix :: Float -> Float -> Float -> Float -> [[Float]]
perspectiveMatrix fov n f ar =
  [ [s/ar, 0,            0,            0]
  , [0,    s,            0,            0]
  , [0,    0, -(f+n)/(f-n), -2*f*n/(f-n)]
  , [0,    0,            1,            0] ] :: [[Float]]
  where s = 1 / (tan (fov/2.0))
