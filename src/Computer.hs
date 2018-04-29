module Computer where

import Data.Array
import System.IO.Unsafe
import System.Random
import Game
import Rendering

r :: IO Int
r = getStdRandom (randomR (0, 200))

z :: [IO Int]
z = [r | a <- [0..200]]

c :: Int -> [Int]
c l = [(unsafePerformIO v) `mod` l | v <- z]

easyMove :: [(Pos, Pos)] -> (Pos, Pos)
easyMove moves = moves !! (head d)
                 where
                 l = length moves
                 d = drop (64 - l) (c l)

updateMarker :: (Pos, Pos) -> Game -> Game
updateMarker (p0, p1) game = game {marker = (marker game) {position = p0, toggled = Just p1}}

playComputer :: Game -> Game
playComputer game = updateMarker positions game
                    where
                    positions = easyMove $ possibleMoves game
