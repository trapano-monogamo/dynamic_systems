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
, module State
, module LinearAlgebra
) where

import LinearAlgebra
import State
import Graphics.Gloss.Interface.Pure.Game



windowSize :: (Int,Int)
windowSize = (1000,1200)

window :: Display
window = InWindow "Dynamic Systems" windowSize (1000,200)

stepSizeToFPS :: Float -> Int
stepSizeToFPS h = round $ 1 / h


renderState :: State -> Picture
renderState state = pictures
  $ (colorStuff $ map (pointToCircle) $ points state)
  ++ (colorStuff $ (map (line) $ trails state))
  ++ debugLogTexts
  where pointToCircle p = (\(x,y) -> translate x y $ circleSolid 10) $ (plotter state) state $ p
        colorStuff stuff =
          if (length $ colors state) == (length stuff) then
            map (\(pic,c) -> color c pic) $ zip stuff (colors state)
          else map (color white) stuff
        debugLogTexts = map
          (\(t,i) -> translate (-w/2.1) (h/2.1 - i*35) $ scale 0.2 0.2 $ color white $ text t)
          $ zip (debugLog state) [0.0,1..]
          where w = fromIntegral $ fst windowSize
                h = fromIntegral $ snd windowSize

interactWithState :: Event -> State -> State
interactWithState (EventKey k kstate _ _) state =
  case (k,kstate) of
    (Char 'z', Down) -> newState $ cameraSetFieldOfView ((fieldOfView cam) / 1.2) cam
    (Char 'Z', Down) -> newState $ cameraSetFieldOfView ((fieldOfView cam) * 1.2) cam
    (_, Down) -> statePressKey k state
    (_, Up) -> stateUnpressKey k state
  where cam = camera state
        newState c = stateSetCamera c state
interactWithState _ state = state

stepState :: Float -> State -> State
stepState t state = State
  { points = map (\(x,idx) -> sol idx t x) $ zip (points state) [0,1..]
  , trails = updateTrails $ trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = system state
  , stepSize = stepSize state
  , solvers = solvers state
  , plotter = plotter state
  , camera = updateCamera $ camera state
  , pressedKeys = pressedKeys state
  , debugLog = (map (show) $ pressedKeys state)
                ++ [show $ pos $ camera state]
                ++ [show $ fieldOfView $ camera state]
  }
  where plot = (plotter state) state

        sol idx = case (solvers state) of
                    SingleSolver s -> s (system state) (stepSize state)
                    MultipleSolvers ss -> (ss !! idx) (system state) (stepSize state)

        updateTrails ts = map (expandTrail) $ zip ts (points state)

        expandTrail (tr,p) =
          if (length tr) <= (trailLimit state) then
            tr ++ [plot p]
          else (tail tr) ++ [plot p]

        updateCamera cam = do
          let dt = stepSize state
              velocity = 2.0
              angVelocity = 0.8
              directions = filter (\x -> x /= [0,0,0]) $ map (\cmd ->
                case cmd of
                  (Char 'w', Down)             -> dir cam
                  (Char 'a', Down)             -> negateV $ right cam
                  (Char 's', Down)             -> negateV $ dir cam
                  (Char 'd', Down)             -> right cam
                  (SpecialKey KeySpace, Down)  -> up cam
                  (SpecialKey KeyShiftL, Down) -> negateV $ up cam
                  _ -> [0,0,0]
                ) $ pressedKeys state
              movement = foldl (\acc d -> ((dt * velocity) .*^ d) ^+^ acc) zeroV directions
              axes = filter (\x -> x /= [0,0,0]) $ map (\cmd ->
                case cmd of
                  (SpecialKey    KeyUp, Down) -> ( angVelocity * dt) .*^ (right cam)
                  (SpecialKey  KeyLeft, Down) -> ( angVelocity * dt) .*^ (up cam)
                  (SpecialKey  KeyDown, Down) -> (-angVelocity * dt) .*^ (right cam)
                  (SpecialKey KeyRight, Down) -> (-angVelocity * dt) .*^ (up cam)
                  _ -> [0,0,0]
                ) $ pressedKeys state
              rotatedCamera = foldl (\c u ->
                cameraSetRight ((rotationMatrix u) **. (right c))
                $ cameraSetUp ((rotationMatrix u) **. (up c))
                $ cameraSetDir ((rotationMatrix u) **. (dir c)) c
                ) cam axes
          cameraSetPos (movement ^+^ (pos cam)) rotatedCamera



{- PLOTTERS -}

phaseSpacePlotter :: State -> [Float] -> (Float,Float)
phaseSpacePlotter _ (x:y:[]) = (x,y)
phaseSpacePlotter _ _ = (0,0)

configSpacePlotter :: state -> [Float] -> (Float,Float)
configSpacePlotter _ (x:_:[]) = (x,0)
configSpacePlotter _ _ = (0,0)

spacePlotter3D :: State -> [Float] -> (Float,Float)
spacePlotter3D state (x:y:z:[]) = transformed
  where nearPlane  = 0.1
        farPlane   = 100.0
        fov  = fieldOfView $ camera state
        p    = pos         $ camera state
        r    = right       $ camera state
        u    = up          $ camera state
        d    = dir         $ camera state
        lookat     = lookatMatrix p r u d
        projection = perspectiveMatrix fov nearPlane farPlane (w/h)
        transformed = result $ (projection .**. lookat) **. [x,y,z,1]
        w = (fromIntegral $ fst windowSize) / 2
        h = (fromIntegral $ snd windowSize) / 2
        result (x':y':_:w':[]) = (w*x' / w', h*y' / w')
          -- if (toBeCulled x' y' z') then (0,0) else (w*x', h*y')
          -- where toBeCulled a b c = (abs c) > 1.0 -- || (abs a) > 1.0 || (abs b) > 1.0
        result _ = (0,0) -- so the compiler doesn't yell at me
spacePlotter3D state (x:y:[]) = spacePlotter3D state [x,y,0]
spacePlotter3D _ _ = (0,0)
