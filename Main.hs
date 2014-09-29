module Main where

import Blinkomatic
import Control.Wire                 ((.), id, (-->))
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TMVar (newEmptyTMVar)
import Prelude                      hiding ((.), id)

main :: IO ()
main =
  do m <- atomically newEmptyTMVar
     openMidi m [] -- ["hw:2,0,0", "hw:2,0,2"]
     clockThread m 120
     blinkomatic m rainbow


