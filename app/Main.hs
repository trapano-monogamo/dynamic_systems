module Main (main) where

import Examples

import Graphics.Gloss.Interface.Pure.Game


{- Hopes and Dreams:
 -
 - 4. multiple points in a single system, or a way to plot different
 -    degrees of freedom as different points
 - 5. implement a way to draw constraints in state/config space as lines/surfaces/...
 -
 - -}



{- *********************************************************************
 - *                                                                   *
 - * WHAT ABOUT systems of points? Currently, a plotter takes          *
 - * state at some time t and outputs a single screen space            *
 - * point, so even though the phase space is able to contain multiple *
 - * points, only one of those can be rendered at any given time       *
 - *                                                                   *
 - *********************************************************************
 - -}


main :: IO ()
main = do
  let h = 0.01
      state = stateSetPoints doublePendulumTestPoints
            $ stateSetSystem doublePendulum
            $ stateSetStepSize h
            $ stateSetSolver rungeKuttaMethod
            $ stateSetPlotter doublePendulumPhasePlotter
            $ stateSetTrailLimit 200
            $ emptyState
  play window black (stepSizeToFPS h) state renderState interactWithState stepState
