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

data Game = Game { gameBoard    :: Board
                 , gamePlayer   :: Player
                 , gameState    :: State
                 , marker       :: Marker
                 , player1Score :: Int
                 , player2Score :: Int
                 , gameWinner   :: Maybe Player
                 , message      :: String
                 , possibleMoves :: Possibilities
                 } deriving (Eq, Show)

-- #TODO: Number of Dots
n :: Int
n = 8

humanMode = 0
computerEasy = 1
gameMode = computerEasy

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 640

boxWidth :: Float
boxWidth = fromIntegral (min screenHeight screenWidth) / fromIntegral (n + 1)

boxHeight :: Float
boxHeight = fromIntegral (min screenHeight screenWidth) / fromIntegral (n + 1)

isValidPair p0 p1 = (abs (fst p0 - fst p1) == 1 && snd p0 == snd p1) ||
                    (abs (snd p0 - snd p1) == 1 && fst p0 == fst p1)

initializePossibleMoves pm =
  [(p0, p1) | (p0, p1) <- indices (pm), isValidPair p0 p1]

-- Game
initialGame =
  Game { gameBoard = array indexRange $ zip (range indexRange) (cycle [Nothing])
         , gamePlayer = Player1
         , gameState = Running
         , marker = Marker (0, 0) Nothing
         , player1Score = 0
         , player2Score = 0
         , gameWinner = Nothing
         , message = "Player1: 0, Player2: 0"
         , possibleMoves = initializePossibleMoves (array indexRange $ zip (range indexRange) (cycle [0]))
         }
         where indexRange = (((0, 0), (0, 0)), ((n-1, n-1), (n-1, n-1)))
