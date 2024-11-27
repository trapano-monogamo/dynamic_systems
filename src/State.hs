module State
( State(..)
, StateSolver(..)
, emptyState
, exampleState
, stateSetPoints
, stateSetTrailLimit
, stateSetSystem
, stateSetStepSize
, stateSetSolver
, stateSetSolvers
, stateSetPlotter
, stateSetCamera
, statePressKey
, stateUnpressKey
, Camera(..)
, cameraSetPos
, cameraSetRight
, cameraSetUp
, cameraSetDir
, cameraSetFieldOfView
, module System
) where

import Graphics.Gloss.Data.Color (Color, green)
-- import Graphics.Gloss.Data.Picture (Path)
import qualified Graphics.Gloss.Interface.Pure.Game as G (Key(..),SpecialKey(..),KeyState(..))

import System


data Camera = Camera
  { pos :: [Float]
  , right :: [Float]
  , up :: [Float]
  , dir :: [Float]
  , fieldOfView :: Float
  , farZ :: Float
  , nearZ :: Float
  }

data StateSolver = SingleSolver Solver | MultipleSolvers [Solver]

data State = State
  { points      :: [[Float]]
  , trails      :: [[[Float]]]
  , trailLimit  :: Int
  , colors      :: [Color]
  , system      :: ODE
  , stepSize    :: Float
  , solvers     :: StateSolver
  , plotter     :: State -> [Float] -> (Float,Float,Float)
  , camera      :: Camera
  , pressedKeys :: [(G.Key,G.KeyState)]
  , debugLog    :: [String]
  }

emptyState :: State
emptyState = State
  { points      = []
  , trails      = []
  , trailLimit  = 1000
  , colors      = []
  , system      = (\_ _ -> zeroV)
  , stepSize    = 0
  , solvers     = SingleSolver (\_ _ _ -> id)
  , plotter     = (\_ _ -> (0,0,0))
  , camera      = initCamera
  , pressedKeys = initKeys
  , debugLog    = []
  }
  where initCamera = Camera { pos         = [0,0,2]
                            , right       = [1,0,0]
                            , up          = [0,1,0]
                            , dir         = [0,0,-1]
                            , fieldOfView = pi / 2.0
                            , farZ        = 10.0
                            , nearZ       = 0.1 }
        initKeys = [ (G.Char 'w', G.Up)
                   , (G.Char 'a', G.Up)
                   , (G.Char 's', G.Up)
                   , (G.Char 'd', G.Up)
                   , (G.SpecialKey G.KeySpace, G.Up)
                   , (G.SpecialKey G.KeyShiftL, G.Up)
                   , (G.SpecialKey G.KeyUp, G.Up)
                   , (G.SpecialKey G.KeyLeft, G.Up)
                   , (G.SpecialKey G.KeyDown, G.Up)
                   , (G.SpecialKey G.KeyRight, G.Up)
                   ]

exampleState :: State
exampleState = State
  { points = [ [100,0] ]
  , trails = []
  , trailLimit = 1000
  , colors = [ green ]
  , system = testSystem 1.0 0.1
  , stepSize = 0.1
  , solvers = SingleSolver rungeKuttaMethod
  , plotter = testPlotter
  , camera = camera emptyState
  , pressedKeys = pressedKeys emptyState
  , debugLog = []
  }
  where testSystem w g _ (x:xdot:[]) = [xdot, -(w**2) * x - g*xdot]
        testSystem _ _ _ _ = [0,0] -- invalid point
        testPlotter _ (x:y:[]) = (x,y,0)
        testPlotter _ _ = (0,0,0)



{- STATE SETTERS -}

stateSetPoints :: [([Float],Color)] -> State -> State
stateSetPoints stuff state = State
  { points = pts
  , trails = map (\x -> [x]) pts
  , trailLimit = trailLimit state
  , colors = cols
  , system = system state
  , stepSize = stepSize state
  , solvers = solvers state
  , plotter = plotter state
  , camera = camera state
  , pressedKeys = pressedKeys state
  , debugLog = debugLog state
  }
  where (pts,cols) = unzip stuff
        plot = plotter state

stateSetTrailLimit :: Int -> State -> State
stateSetTrailLimit tl state = State
  { points = points state
  , trails = trails state
  , trailLimit = tl
  , colors = colors state
  , system = system state
  , stepSize = stepSize state
  , solvers = solvers state
  , plotter = plotter state
  , camera = camera state
  , pressedKeys = pressedKeys state
  , debugLog = debugLog state
  }

stateSetSystem :: ODE -> State -> State
stateSetSystem f state = State
  { points = points state
  , trails = trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = f
  , stepSize = stepSize state
  , solvers = solvers state
  , plotter = plotter state
  , camera = camera state
  , pressedKeys = pressedKeys state
  , debugLog = debugLog state
  }

stateSetStepSize :: Float -> State -> State
stateSetStepSize h state = State
  { points = points state
  , trails = trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = system state
  , stepSize = h
  , solvers = solvers state
  , plotter = plotter state
  , camera = camera state
  , pressedKeys = pressedKeys state
  , debugLog = debugLog state
  }

stateSetSolver :: Solver -> State -> State
stateSetSolver s state = State
  { points = points state
  , trails = trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = system state
  , stepSize = stepSize state
  , solvers = SingleSolver s
  , plotter = plotter state
  , camera = camera state
  , pressedKeys = pressedKeys state
  , debugLog = debugLog state
  }

stateSetSolvers :: [Solver] -> State -> State
stateSetSolvers ss state = State
  { points = points state
  , trails = trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = system state
  , stepSize = stepSize state
  , solvers = MultipleSolvers ss
  , plotter = plotter state
  , camera = camera state
  , pressedKeys = pressedKeys state
  , debugLog = debugLog state
  }

stateSetPlotter :: (State -> [Float] -> (Float,Float,Float)) -> State -> State
stateSetPlotter p state = State
  { points = points state
  , trails = trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = system state
  , stepSize = stepSize state
  , solvers = solvers state
  , plotter = p
  , camera = camera state
  , pressedKeys = pressedKeys state
  , debugLog = debugLog state
  }

stateSetConstraint :: [[Float]] -> State -> State
stateSetConstraint c state = State
  { points = points state
  , trails = trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = system state
  , stepSize = stepSize state
  , solvers = solvers state
  , plotter = plotter state
  , camera = camera state
  , pressedKeys = pressedKeys state
  , debugLog = debugLog state
  }

stateSetCamera :: Camera -> State -> State
stateSetCamera cam state = State
  { points = points state
  , trails = trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = system state
  , stepSize = stepSize state
  , solvers = solvers state
  , plotter = plotter state
  , camera = cam
  , pressedKeys = pressedKeys state
  , debugLog = debugLog state
  }

statePressKey :: G.Key -> State -> State
statePressKey k state = State
  { points = points state
  , trails = trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = system state
  , stepSize = stepSize state
  , solvers = solvers state
  , plotter = plotter state
  , camera = camera state
  , pressedKeys = updatedKeys
  , debugLog = debugLog state
  }
  where updatedKeys = map (\(kk,s) -> if kk == k then (kk,G.Down) else (kk,s))
                      $ pressedKeys state

stateUnpressKey :: G.Key -> State -> State
stateUnpressKey k state = State
  { points = points state
  , trails = trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = system state
  , stepSize = stepSize state
  , solvers = solvers state
  , plotter = plotter state
  , camera = camera state
  , pressedKeys = updatedKeys
  , debugLog = debugLog state
  }
  where updatedKeys = map (\(kk,s) -> if kk == k then (kk,G.Up) else (kk,s))
                      $ pressedKeys state


{- END STATE SETTERS -}


{- CAMERA SETTERS -}

cameraSetPos :: [Float] -> Camera -> Camera
cameraSetPos p cam = Camera
  { pos = p
  , right = right cam
  , up = up cam
  , dir = dir cam
  , fieldOfView = fieldOfView cam
  , farZ = farZ cam
  , nearZ = nearZ cam
  }

cameraSetRight :: [Float] -> Camera -> Camera
cameraSetRight r cam = Camera
  { pos = pos cam
  , right = r
  , up = up cam
  , dir = dir cam
  , fieldOfView = fieldOfView cam
  , farZ = farZ cam
  , nearZ = nearZ cam
  }

cameraSetUp :: [Float] -> Camera -> Camera
cameraSetUp u cam = Camera
  { pos = pos cam
  , right = right cam
  , up = u
  , dir = dir cam
  , fieldOfView = fieldOfView cam
  , farZ = farZ cam
  , nearZ = nearZ cam
  }

cameraSetDir :: [Float] -> Camera -> Camera
cameraSetDir d cam = Camera
  { pos = pos cam
  , right = right cam
  , up = up cam
  , dir = d
  , fieldOfView = fieldOfView cam
  , farZ = farZ cam
  , nearZ = nearZ cam
  }

cameraSetFieldOfView :: Float -> Camera -> Camera
cameraSetFieldOfView fov cam = Camera
  { pos = pos cam
  , right = right cam
  , up = up cam
  , dir = dir cam
  , fieldOfView = fov
  , farZ = farZ cam
  , nearZ = nearZ cam
  }

{- END CAMERA SETTERS -}
