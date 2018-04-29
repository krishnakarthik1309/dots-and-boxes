module Computer where

import Data.Array
{-import Data.Functor-}
import Data.Random
import System.Random
import Game
import Rendering

getPossibleMoves :: Game -> [(Pos, Pos)]
getPossibleMoves game = [i | i <- indices (possibleMoves game), let x = (possibleMoves game)!i, x == 0]

easyMove :: [(Pos, Pos)] -> (Pos, Pos)
{-easyMove moves = do g <- newStdGen-}
                    {-moves !! (fst $ randomR (0, length moves - 1) g)-}
{-easyMove moves = (moves !!) <$> randomRIO (0, length moves - 1)-}
{-easyMove moves = ((5,5),(5,6))-}
easyMove moves = sample $ randomElement moves
{-easyMove moves = head . unfoldr (Just )-}

updateMarker :: (Pos, Pos) -> Game -> Game
updateMarker (p0, p1) game = game {marker = (marker game) {position = p0, toggled = Just p1}}

playComputer :: Game -> Game
playComputer game = updateMarker positions game
                    where
                    positions = easyMove $ getPossibleMoves game
