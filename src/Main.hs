module Main where

import Graphics.Gloss

window = InWindow "Dots and Boxes" (200, 200) (10, 10)

backgroundColor = black

main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)