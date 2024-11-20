module Main (main) where

import Examples

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


main :: IO ()
main = do
  let h = 0.01
      state = stateSetPoints sphericalPendulumTestPoints
              $ stateSetSystem sphericalPendulum
              $ stateSetStepSize h
              $ stateSetSolver rungeKuttaMethod
              $ stateSetPlotter sphericalPendulumPlotter
              $ stateSetTrailLimit 1000
              $ emptyState
  play window black (stepSizeToFPS h) state renderState interactWithState stepState
