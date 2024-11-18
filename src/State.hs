module State
( State(..)
, StateSolver(..)
, setPoints
, setTrailLimit
, setSystem
, setStepSize
, setSolver
, setSolvers
, setPlotter
, emptyState
, exampleState
) where

import System
import Graphics.Gloss.Data.Color (Color, green)
import Graphics.Gloss.Data.Picture (Path)



data Camera = Camera
  { pos :: [Float]
  , up :: [Float]
  , dir :: [Float]
  , right :: [Float]
  , fov :: Float
  , farZ :: Float
  , nearZ :: Float
  }

data StateSolver = SingleSolver Solver | MultipleSolvers [Solver]

data State = State
  { points :: [[Float]]
  , trails :: [Path]
  , trailLimit :: Int
  , colors :: [Color]
  , system :: ODE
  , stepSize :: Float
  , solvers :: StateSolver
  , plotter :: [Float] -> (Float,Float)
  -- , camera :: Camera
  }

emptyState :: State
emptyState = State
  { points = []
  , trails = []
  , trailLimit = 1000
  , colors = []
  , system = (\_ -> id)
  , stepSize = 0
  , solvers = SingleSolver (\_ _ _ -> id)
  , plotter = (\_ -> (0,0))
  }

exampleState :: State
exampleState = State
  { points = [ [100,0] ]
  , trails = [ [(100,0)] ]
  , trailLimit = 1000
  , colors = [ green ]
  , system = testSystem 1.0 0.1
  , stepSize = 0.1
  , solvers = SingleSolver rungeKuttaMethod
  , plotter = testPlotter
  }
  where testSystem w g _ (x:xdot:[]) = [xdot, -(w**2) * x - g*xdot]
        testSystem _ _ _ _ = [0,0] -- invalid point
        testPlotter (x:y:[]) = (x,y)
        testPlotter _ = (0,0)



{- SETTERS -}

setPoints :: [([Float],Color)] -> State -> State
setPoints stuff state = State
  { points = pts
  , trails = map (\x -> [plot x]) pts
  , trailLimit = trailLimit state
  , colors = cols
  , system = system state
  , stepSize = stepSize state
  , solvers = solvers state
  , plotter = plotter state
  }
  where (pts,cols) = unzip stuff
        plot = plotter state

setTrailLimit :: Int -> State -> State
setTrailLimit tl state = State
  { points = points state
  , trails = trails state
  , trailLimit = tl
  , colors = colors state
  , system = system state
  , stepSize = stepSize state
  , solvers = solvers state
  , plotter = plotter state
  }

setSystem :: ODE -> State -> State
setSystem f state = State
  { points = points state
  , trails = trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = f
  , stepSize = stepSize state
  , solvers = solvers state
  , plotter = plotter state
  }

setStepSize :: Float -> State -> State
setStepSize h state = State
  { points = points state
  , trails = trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = system state
  , stepSize = h
  , solvers = solvers state
  , plotter = plotter state
  }

setSolver :: Solver -> State -> State
setSolver s state = State
  { points = points state
  , trails = trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = system state
  , stepSize = stepSize state
  , solvers = SingleSolver s
  , plotter = plotter state
  }

setSolvers :: [Solver] -> State -> State
setSolvers ss state = State
  { points = points state
  , trails = trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = system state
  , stepSize = stepSize state
  , solvers = MultipleSolvers ss
  , plotter = plotter state
  }

setPlotter :: ([Float] -> (Float,Float)) -> State -> State
setPlotter p state = State
  { points = points state
  , trails = trails state
  , trailLimit = trailLimit state
  , colors = colors state
  , system = system state
  , stepSize = stepSize state
  , solvers = solvers state
  , plotter = p
  }

{- END SETTERS -}
