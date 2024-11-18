module Main (main) where

import LinearAlgebra
import System
import State
import Rendering

import Graphics.Gloss.Interface.Pure.Game


{- Hopes and Dreams:
 -
 - 4. implement rudimentary 3D as a simple global transform in State
 -  -> create a KSaceOperator class to abstract matrices that act
 -     on KVectorSpace, then implement a transform: view and projection
 -  -> maybe have an option to State that enables 3D by setting the global
 -     transform to an identity (no view and no projection)
 - 5. implement a way to draw constraints in state/config space as lines/surfaces/...
 -
 - -}


{- Harmonic Oscillator -}

harmonicOscillator :: Float -> ODE
harmonicOscillator w _ (x:xdot:[]) = [xdot, -(w**2) * x]
harmonicOscillator _ _ _ = [0,0] -- invalid point

dampedHarmonicOscillator :: Float -> Float -> ODE
dampedHarmonicOscillator w g _ (x:xdot:[]) = [xdot, -(w**2) * x - g*xdot]
dampedHarmonicOscillator _ _ _ _ = [0,0] -- invalid point



{- Lotka-Volterra -}

-- good parameters: 
-- a = 2
-- b = 0.1
-- c = 1
-- d = 0.1

lotkaVolterra :: Float -> Float -> Float -> Float -> ODE
lotkaVolterra a b c d t (x:y:[]) =
  [  a*x - b*x*y
  , -c*y + d*x*y
  ]
lotkaVolterra _ _ _ _ _ _ = [0,0]

lotkaVolterraPlotter :: [Float] -> (Float,Float)
lotkaVolterraPlotter (x:y:[]) =
  ( 10 * x - (fromIntegral $ ((fst windowSize) `div` 2))
  , 10 * y - (fromIntegral $ ((snd windowSize) `div` 2)) )

lotkaVolterraTestPoints :: [([Float],Color)]
lotkaVolterraTestPoints = map (ptAndCol) [20.0,25..95]
  where ptAndCol x = ( [10,x], makeColor (x/95.0) (1 - x/95.0) (1.0) (1.0) )



{- Lorentz Attractor -}

-- good parameters:
-- a = 10
-- b = variable
-- c = 8/3

lorentzAttractor :: Float -> Float -> Float -> ODE
lorentzAttractor a b c t (x:y:z:[]) =
  [ a*(y - x)
  , b*x - x*z - y
  , x*y - c*z ]
lorentzAttractor _ _ _ _ _ = [0,0,0]

lorentzAttractorTestPoints :: [([Float],Color)]
lorentzAttractorTestPoints = [ ([0.1,-10,1.3],white) ]

lorentzAttractorPlotter :: [Float] -> (Float,Float)
lorentzAttractorPlotter (_:y:z:[]) = (30 * y, 30 * z)
lorentzAttractorPlotter _ = (0,0)



main :: IO ()
main = do
  let stepSize = 0.01
      state = setPoints lotkaVolterraTestPoints
              $ setSystem (lotkaVolterra 2 0.1 1 0.1)
              $ setStepSize stepSize
              $ setSolver rungeKuttaMethod
              $ setPlotter lotkaVolterraPlotter -- spacePlotter3D
              $ setTrailLimit 2000
              $ emptyState
  play window black (stepSizeToFPS stepSize) state renderState interactWithState stepState
