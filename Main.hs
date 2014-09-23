module Main where

import Blinkomatic
import Control.Wire ((.), id)
import Prelude      hiding ((.), id)

main :: IO ()
main = blinkomatic "hw:2,0,0" "hw:2,0,2" (sequences . modeSwitcher)

