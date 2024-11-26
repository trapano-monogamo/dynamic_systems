module Examples
( harmonicOscillator
, dampedHarmonicOscillator

, lotkaVolterra
, lotkaVolterraPlotter
, lotkaVolterraTestPoints

, lorentzAttractor
, lorentzAttractorPlotter
, lorentzAttractorTestPoints

, centrifugalGovernor
, centrifugalGovernorPlotter
, centrifugalGovernorTestPoints

, sphericalPendulum
, sphericalPendulumPlotter
, sphericalPendulumTestPoints

, rutherfordScattering
, rutherfordScatteringPlotter
, rutherfordScatteringTestPoints

, doublePendulum
, doublePendulumConfigPlotter
, doublePendulumPhasePlotter
, doublePendulumTestPoints

, module Rendering
) where

import Graphics.Gloss.Data.Color (Color, makeColor, white, red, green, blue)

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



{- Centrifugal Governor -}

centrifugalGovernor :: ODE
centrifugalGovernor _ (phi:phi':theta:theta':[]) =
  [ l / (2 * m * len**2 * (sin theta)**2)
  , 0
  , theta'
  , (l**2 * cos theta) / (4 * m**2 * l**4 * (sin theta)**3) - (g/l) * (sin theta)
  ]
  where l = 2
        m = 1
        len = 1
        g = 9.81
centrifugalGovernor _ _ = [0,0,0,0] -- invalid point

centrifugalGovernorPlotter :: State -> [Float] -> (Float,Float,Float)
centrifugalGovernorPlotter state (phi:_:theta:_:[]) = spacePlotter3D state [x,y,z]
  where x =  l * (sin phi) * (sin theta)
        y = -l * (cos theta)
        z =  l * (cos phi) * (sin theta)
        l = 1
-- centrifugalGovernorPlotter state (phi:_:theta:_:[]) = (x,y,0)
--   where x =  l * (sin phi) * (sin theta)
--         y = -l * (cos theta)
--         l = 1
centrifugalGovernorPlotter _ _ = (0,0,0)

centrifugalGovernorTestPoints :: [([Float],Color)]
centrifugalGovernorTestPoints =
  [ ([0.0, phi', theta, 0.0], makeColor 1.0 0.0 1.0 1.0)
  , ([ pi, phi', theta, 0.0], makeColor 0.0 1.0 1.0 1.0) ]
  where l = 2
        m = 1
        len = 1
        theta = 1.5
        phi' = l / (2 * m * len**2 * (sin theta)**2)



{- Spherical Pendulum -}

sphericalPendulum :: ODE
sphericalPendulum _ (_:phi':theta:theta':[]) =
  [ l / (len**2 * (sin theta)**2)
  , 0
  , theta'
  , (l**2 / len**4) * ((cos theta) / ((sin theta) ** 3)) - (g/len) * (sin theta)
  ]
  where g = 9.81
        l = 1.0 -- !!! this way the initial velocities set for the test points don't matter, but HOW THE HELL do I make different points have different angular momentums
        len = 1.0
sphericalPendulum _ _ = [0,0,0,0] -- invalid point

sphericalPendulumPlotter :: State -> [Float] -> (Float,Float,Float)
sphericalPendulumPlotter state (phi:_:theta:_:[]) = spacePlotter3D state [x,y,z]
  where z =  l * (cos phi) * (sin theta) -- x
        x =  l * (sin phi) * (sin theta) -- y
        y = -l * (cos theta) -- z
        l = 1.0
sphericalPendulumPlotter _ _ = (0,0,0)

sphericalPendulumTestPoints :: [([Float],Color)]
sphericalPendulumTestPoints = ((map (makePoint) [0.0, 0.4 .. pi / 1.5]) ++ [equilibrium])
  where makePoint s = ( [0.0, 1.0, s, 0.0]
                      , makeColor (s*1.5/pi) (1-s*1.5/pi) 1.0 1.0 )
        -- equilibrium theta for len=1, angularMomentum=1, mass=1
        equilibrium = ( [0, 1/(sin 0.571743)**2, 0.571743, 0]
                      , white )



{- Rutherford Scattering -}

rutherfordScattering :: ODE
rutherfordScattering _ (r:r':theta:theta':l:l':[]) =
  [ r'
  , -(1/m) * (effPot' r)
  , l / (m * r**2)
  , 0
  , 0
  , 0
  ]
  where effPot' x = -(k/x**2) + l**2 / (m * x**3)
        m = 1
        k = 3
rutherfordScattering _ _ = [0,0,0,0,0,0] -- invalid point

rutherfordScatteringPlotter :: State -> [Float] -> (Float,Float,Float)
rutherfordScatteringPlotter state (r:_:theta:_:_:_:[]) = (x,y,0)
  where x = r * cos theta
        y = r * sin theta
rutherfordScatteringPlotter _ _ = (0,0,0)

rutherfordScatteringTestPoints :: [([Float],Color)]
rutherfordScatteringTestPoints =
  [ ([r, r', theta, theta', l, 0], makeColor 1.0 0.0 0.0 1.0)
  ]
  where m = 1
        k = 3
        r = 0.5
        theta = pi
        r' = -1.0
        theta' = 0.1
        l = theta' * m * r**2
        -- effPot x = k/x - l**2 / (2 * m * x**2)
        -- l = 1.4
        -- r = 0.5
        -- theta = 0.8*pi
        -- r' = sqrt $ (2/m) * (energyLevel - effPotential r)
        -- theta' = l / (m * r**2)
        -- energyLevel = (m/2) * (r'**2 + r**2 * theta'**2) + (effPotential r)



{- Double Pendulum -}

doublePendulum :: ODE
doublePendulum _ (theta:theta':phi:phi':idx:[]) =
  [ theta'
  , (b * (cos $ theta + phi) - a) / (2 - (cos $ theta + phi)**2)
  , phi'
  , -((b * (cos $ theta + phi) - a) / (2 - (cos $ theta + phi)**2)) * (cos $ theta + phi) - b
  , 0
  ]
  where a = -(phi')**2 * (sin $ theta + phi) + 2 * (g/l) * (sin theta)
        b = -(theta')**2 * (sin $ theta + phi) + (g/l) * (sin phi)
        g = 9.81
        l = 1
doublePendulu _ _ = [0,0,0,0]

doublePendulumConfigPlotter :: State -> [Float] -> (Float,Float,Float)
doublePendulumConfigPlotter state (theta:_:phi:_:idx:[]) = (x/2,y/2,0)
  -- spacePlotter3D state [(sin phi) + (sin theta), -(cos phi) - (cos theta), 0]
  where x = if idx == 0 then (sin theta)    else (sin theta) + (sin phi)
        y = if idx == 0 then (-(cos theta)) else (-(cos theta) -(cos phi))
doublePendulumConfigPlotter _ _ = (0,0,0)

doublePendulumPhasePlotter :: State -> [Float] -> (Float,Float,Float)
doublePendulumPhasePlotter state (theta:theta':phi:phi':idx:[]) = (x/10,y/10,0)
  -- spacePlotter3D state [(sin phi) + (sin theta), -(cos phi) - (cos theta), 0]
  where x = if idx == 0 then theta else phi
        y = if idx == 0 then theta' else phi'
doublePendulumPhasePlotter _ _ = (0,0,0)

doublePendulumTestPoints :: [ ([Float],Color) ]
doublePendulumTestPoints = (concat $ map makePoint [0.0,0.0001..0.001]) ++ equilibrium
  where makePoint s = [ ([pi/2.0, 0.0, pi, -s, 0.0], makeColor (1 - s/0.001) 0.0 (s/0.001) 1.0)
                      , ([pi/2.0, 0.0, pi, -s, 1.0], makeColor (s/0.001) (1 - s/0.001) 0.0 1.0) ]
        equilibrium = [ ([pi/8.0, 0.0, -pi/8.0, 0.0, 0.0], white)
                      , ([pi/8.0, 0.0, -pi/8.0, 0.0, 1.0], white)
                      ]
