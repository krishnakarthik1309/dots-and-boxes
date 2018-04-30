module Computer where

import Data.Array
import System.IO.Unsafe
import System.Random
import Game
import Rendering

r :: Int -> IO Int
r n = getStdRandom (randomR (0, (max (4*n*n) 200)))

z :: Int -> [IO Int]
z n = [r n | a <- [0..(max (4*n*n) 200)]]

c :: Int -> Int -> [Int]
c l n = [(unsafePerformIO v) `mod` l | v <- z n]

easyMove :: [(Pos, Pos)] -> Int -> (Pos, Pos)
easyMove moves n = moves !! (head d)
                     where
                     l = (length moves) - 1
                     d = drop (64 - l) (c l n)

updateMarker :: (Pos, Pos) -> Game -> Game
updateMarker (p0, p1) game = game {marker = (marker game) {position = p0, toggled = Just p1}}

playComputer :: Game -> Game
playComputer game = updateMarker positions game
                    where
                    positions = easyMove (possibleMoves game) (numDots game)
