module Game where

import Data.Array

data Player = Player1 | Player2 deriving (Eq, Show)
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)
data Dot = Selected | NotSelected deriving (Eq, Show)

type Pos = (Int, Int)
type Board = Array (Pos, Pos) Dash
type Possibilities = [(Pos, Pos)]
type Box = Maybe Player
type Dash = Maybe Player

data Marker = Marker { position :: Pos
                     , toggled  :: Maybe Pos} deriving (Eq, Show)

data Game = Game { gameBoard     :: Board
                 , gamePlayer    :: Player
                 , gameState     :: State
                 , marker        :: Marker
                 , player1Score  :: Int
                 , player2Score  :: Int
                 , gameWinner    :: Maybe Player
                 , message       :: String
                 , possibleMoves :: Possibilities
                 , numDots       :: Int
                 , gameMode      :: Int
                 } deriving (Eq, Show)

-- CONSTANTS
humanMode = 0 :: Int
computerEasy = 1 :: Int

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 640

boxWidth :: Game -> Float
boxWidth game =
  fromIntegral (min screenHeight screenWidth) / fromIntegral (n + 1)
  where n = (numDots game)

boxHeight :: Game -> Float
boxHeight game =
  fromIntegral (min screenHeight screenWidth) / fromIntegral (n + 1)
  where n = (numDots game)

isValidPair p0 p1 = (abs (fst p0 - fst p1) == 1 && snd p0 == snd p1) ||
                    (abs (snd p0 - snd p1) == 1 && fst p0 == fst p1)

initializePossibleMoves pm =
  [(p0, p1) | (p0, p1) <- indices (pm), isValidPair p0 p1]

-- Game
initialGame numDots gameMode =
  Game { gameBoard = array indexRange $ zip (range indexRange) (cycle [Nothing])
       , gamePlayer = Player1
       , gameState = Running
       , marker = Marker (0, 0) Nothing
       , player1Score = 0
       , player2Score = 0
       , gameWinner = Nothing
       , message = "Player1: 0, Player2: 0"
       , possibleMoves = initializePossibleMoves (array indexRange $ zip (range indexRange) (cycle [0]))
       , numDots = numDots
       , gameMode = gameMode
       }
       where
        n = numDots
        indexRange = (((0, 0), (0, 0)), ((n-1, n-1), (n-1, n-1)))