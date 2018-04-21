module Game where

import Data.Array

data Player = Player1 | Player2 deriving (Eq, Show)
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)
data Dot = Selected | NotSelected deriving (Eq, Show)

type Pos = (Int, Int)
type Board = Array (Pos, Pos) Dash
type Box = Maybe Player
type Dash = Maybe Player

data Marker = Marker { position :: Pos
                     , toggled  :: Maybe Pos} deriving (Eq, Show)

data Game = Game { gameBoard    :: Board
                 , gamePlayer   :: Player
                 , gameState    :: State
                 , marker       :: Marker
                 , player1Score :: Int
                 , player2Score :: Int
                 , gameWinner   :: Maybe Player
                 } deriving (Eq, Show)

-- #TODO: Number of Dots
n :: Int
n = 8

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 640

boxWidth :: Float
boxWidth = fromIntegral (min screenHeight screenWidth) / fromIntegral (n + 1)

boxHeight :: Float
boxHeight = fromIntegral (min screenHeight screenWidth) / fromIntegral (n + 1)

-- Game
initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (cycle [Nothing])
                   , gamePlayer = Player1
                   , gameState = Running
                   , marker = Marker (0, 0) Nothing
                   , player1Score = 0
                   , player2Score = 0
                   , gameWinner = Nothing
                   }
                   where indexRange = (((0, 0), (0, 0)), ((n-1, n-1), (n-1, n-1)))
