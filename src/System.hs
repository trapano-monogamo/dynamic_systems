module System
( ODE
, Solver
, eulerMethod
, rungeKuttaMethod
, module LinearAlgebra
) where

import LinearAlgebra

-- ODE    : t, x -> xdot
-- Solver : ODE, stepsize -> (\ t,x -> x' )

type ODE = Float -> [Float] -> [Float]
type Solver = ODE -> Float -> (Float -> [Float] -> [Float])


eulerMethod :: Solver
eulerMethod f h t x = x ^+^ (h .*^ f t x)

rungeKuttaMethod :: Solver
rungeKuttaMethod f h t x = x ^+^ ((k1 ^+^ (2 .*^ k2) ^+^ (2 .*^ k3) ^+^ k4) ^*. (h/6))
  where k1 = f   t          x
        k2 = f  (t + h/2)  (x ^+^ (k1 ^*. (h/2)))
        k3 = f  (t + h/2)  (x ^+^ (k2 ^*. (h/2)))
        k4 = f  (t + h)    (x ^+^ (k3 ^*.  h))
