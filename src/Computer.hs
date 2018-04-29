module Computer where

import Data.Array
import System.IO.Unsafe
import System.Random
import Game
import Rendering

getPossibleMoves :: Game -> [(Pos, Pos)]
getPossibleMoves game = [i | i <- indices (possibleMoves game), let x = (possibleMoves game)!i, x == 0]

r :: IO Int
r = getStdRandom (randomR (0, 200))

z :: [IO Int]
z = [r | a <- [0..200]]

c :: Int -> [Int]
c l = [(unsafePerformIO v) `mod` l | v <- z]

easyMove :: [(Pos, Pos)] -> (Pos, Pos)
{-easyMove moves = do g <- newStdGen-}
                    {-moves !! (fst $ randomR (0, length moves - 1) g)-}
{-easyMove moves = (moves !!) <$> randomRIO (0, length moves - 1)-}
{-easyMove moves = ((5,5),(5,6))-}
{-easyMove moves = sample $ randomElement moves-}
{-easyMove moves = head . unfoldr (Just )-}
{-easyMove moves = (moves !!) (unsafePerformIO (r (length moves - 1)))-}
easyMove moves = moves !! (head d)
                 where
                 l = length moves
                 d = drop (64 - l) (c l)

getAdjacent :: Board -> Pos -> Pos
getAdjacent board p0 = if (snd p0 + 1 < 8 && board ! (p0, (fst p0, snd p0 + 1)) == Nothing)
                        then (fst p0, snd p0 + 1)
                        else if (snd p0 - 1 < 8 && board ! (p0, (fst p0, snd p0 - 1)) == Nothing)
                                  then (fst p0, snd p0 - 1)
                                  else if (fst p0 + 1 < 8 && board ! (p0, (fst p0 + 1, snd p0)) == Nothing)
                                    then (fst p0 + 1, snd p0)
                                    else if (fst p0 - 1 < 8 && board ! (p0, (fst p0 - 1, snd p0)) == Nothing)
                                        then (fst p0 - 1, snd p0)
                                        else p0

updateMarker :: (Pos, Pos) -> Game -> Game
updateMarker (p0, p1) game = game {marker = (marker game) {position = p1, toggled = Just (getAdjacent (gameBoard game) p1)}}

playComputer :: Game -> Game
playComputer game = updateMarker positions game
                    where
                    positions = easyMove $ getPossibleMoves game
