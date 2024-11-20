module Examples
( harmonicOscillator
, dampedHarmonicOscillator

, lotkaVolterra
, lotkaVolterraPlotter
, lotkaVolterraTestPoints

, lorentzAttractor
, lorentzAttractorPlotter
, lorentzAttractorTestPoints

, sphericalPendulum
, sphericalPendulumPlotter
, sphericalPendulumTestPoints

, module Rendering
) where

import Graphics.Gloss.Data.Color (Color, makeColor, white)

import State
import Rendering


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
lotkaVolterra a b c d _ (x:y:[]) =
  [  a*x - b*x*y
  , -c*y + d*x*y
  ]
lotkaVolterra _ _ _ _ _ _ = [0,0] -- invalid point

lotkaVolterraPlotter :: State -> [Float] -> (Float,Float,Float)
lotkaVolterraPlotter _ (x:y:[]) =
  ( 10 * x - (fromIntegral $ ((fst windowSize) `div` 2))
  , 10 * y - (fromIntegral $ ((snd windowSize) `div` 2))
  , 0)
lotkaVolterraPlotter _ _ = (0,0,0)

lotkaVolterraTestPoints :: [([Float],Color)]
lotkaVolterraTestPoints = map (ptAndCol) [20.0,25..95]
  where ptAndCol x = ( [10,x], makeColor (x/95.0) (1 - x/95.0) (1.0) (1.0) )



{- Lorentz Attractor -}

-- good parameters:
-- a = 10
-- b = variable
-- c = 8/3

lorentzAttractor :: Float -> Float -> Float -> ODE
lorentzAttractor a b c _ (x:y:z:[]) =
  [ a*(y - x)
  , b*x - x*z - y
  , x*y - c*z ]
lorentzAttractor _ _ _ _ _ = [0,0,0] -- invalid point

lorentzAttractorPlotter :: State -> [Float] -> (Float,Float,Float)
lorentzAttractorPlotter _ (_:y:z:[]) = (30 * y, 30 * z,0)
lorentzAttractorPlotter _ _ = (0,0,0)

lorentzAttractorTestPoints :: [([Float],Color)]
lorentzAttractorTestPoints = [ ([0.1,-10,1.3],white) ]



{- Spherical Pendulum -}

-- easy mass: m = 1
-- gravity: g = 9.81

-- phi is a cyclical coordinate (phi and phi' are not used => _)
sphericalPendulum :: ODE
sphericalPendulum _ (_:_:theta:theta':[]) =
  [ l / (len**2 * (sin theta) ** 2)
  , 0
  , theta'
  , (l**2 / len**4) * ((cos theta) / ((sin theta) ** 3)) - (g/len) * (sin theta)
  ]
  where g = 9.81
        l = 1
        len = 1.0
sphericalPendulum _ _ = [0,0,0,0] -- invalid points

sphericalPendulumPlotter :: State -> [Float] -> (Float,Float,Float)
sphericalPendulumPlotter state (phi:_:theta:_:[]) = spacePlotter3D state [x,y,z]
  where z =  l * (cos phi) * (sin theta) -- x
        x =  l * (sin phi) * (sin theta) -- y
        y = -l * (cos theta) -- z
        l = 1.0
sphericalPendulumPlotter _ _ = (0,0,0)

sphericalPendulumTestPoints :: [([Float],Color)]
sphericalPendulumTestPoints = ((map (makePoint) [0.0, 0.3 .. pi / 1.5]) ++ [equilibrium])
  where makePoint s = ( [0.0, (s*2/pi), s, 0.0]
                      , makeColor (s*1.5/pi) (1-s*1.5/pi) 1.0 1.0)
        -- equilibrium theta for len=1, angularMomentum=1, mass=1
        equilibrium = ( [0, 1/(sin 0.571743)**2, 0.571743, 0]
                      , white )
