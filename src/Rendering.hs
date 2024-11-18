module Rendering
( window
, windowSize
, stepSizeToFPS
, renderState
, interactWithState
, stepState
, phaseSpacePlotter
, configSpacePlotter
, spacePlotter3D
) where

import LinearAlgebra
import State
import Graphics.Gloss.Interface.Pure.Game



window :: Display
window = InWindow "Dynamic Systems" windowSize (1000,200)

windowSize :: (Int,Int)
windowSize = (1000,1200)

fieldOfView = pi/2.0

stepSizeToFPS :: Float -> Int
stepSizeToFPS h = round $ 1 / h


renderState :: State -> Picture
renderState state = pictures
  $ (colorStuff $ map (pointToCircle) $ points state)
  ++ (colorStuff $ (map (line) $ trails state))
  where pointToCircle p = (\(x,y) -> translate x y $ circleSolid 10) $ plotter state $ p
        colorStuff stuff =
          if (length $ colors state) == (length stuff) then
            map (\(pic,c) -> color c pic) $ zip stuff (colors state)
          else map (color white) stuff

interactWithState :: Event -> State -> State
interactWithState _ = id


stepState :: Float -> State -> State
stepState t state = State
  { points = map (\(x,idx) -> sol idx t x) $ zip (points state) [0,1..]
  , trails = updateTrails $ trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = system state
  , stepSize = stepSize state
  , solvers = solvers state
  , plotter = plot
  }
  where plot = plotter state
        sol :: Int -> Float -> [Float] -> [Float]
        sol idx = case (solvers state) of
                    SingleSolver s -> s (system state) (stepSize state)
                    MultipleSolvers ss -> (ss !! idx) (system state) (stepSize state)
        updateTrails ts = map (expandTrail) $ zip ts (points state)
        expandTrail (t,p) = if (length t) <= (trailLimit state) then t ++ [plotter state $ p] else (tail t) ++ [plotter state $ p]



{- PLOTTERS -}

phaseSpacePlotter :: [Float] -> (Float,Float)
phaseSpacePlotter (x:y:[]) = (x,y)
phaseSpacePlotter _ = (0,0)

configSpacePlotter :: [Float] -> (Float,Float)
configSpacePlotter (x:_:[]) = (x,0)
configSpacePlotter _ = (0,0)

spacePlotter3D :: [Float] -> (Float,Float)
spacePlotter3D (x:y:z:[]) = (x',y')
  where far = 10.0
        near = 0.1
        fov = fieldOfView
        s = 1 / (tan (fov/2.0))
        view = [ [1,0,0,0]
               , [0,1,0,0]
               , [0,0,1,0]
               , [0,0,0,1] ] :: [[Float]]
        projection = [ [s, 0,                     0,  0]
                     , [0, s,                     0,  0]
                     , [0, 0,      - far/(far-near), -1]
                     , [0, 0, - far*near/(far-near),  0] ] :: [[Float]]
        (x':y':_:_:[]) = (projection .**. view) **. [x,y,1,1]
spacePlotter3D (x:y:[]) = spacePlotter3D [x,y,1]
spacePlotter3D _ = (0,0)
