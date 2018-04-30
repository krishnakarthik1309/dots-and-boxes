module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Logic
import Rendering

window = InWindow "Dots and Boxes" (screenWidth, screenHeight) (100, 100)

backgroundColor = makeColor 0 0 0 255

main :: IO ()
main = do
        numDotsString <- getLine
        let numDots = (read numDotsString :: Int)
        play window backgroundColor 30 (initialGame numDots) gameAsPicture transformGame (const id)