module Game where

import Data.Array

data Player = Player1 | Player2 deriving (Eq, Show)
data State = Running | GameOver Player deriving (Eq, Show)
data Dot = Selected | NotSelected deriving (Eq, Show)

type Board = Array (Int, Int) Dot
type Box = Maybe Player

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)

-- #TODO: Number of Dots
n :: Int
n = 8

-- Game
initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (cycle [NotSelected])
                   , gamePlayer = Player1
                   , gameState = Running
                   }
                   where indexRange = ((0, 0), (n-1, n-1))
