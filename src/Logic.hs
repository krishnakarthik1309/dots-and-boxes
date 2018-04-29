module Logic where

import Game
import Rendering
import Computer
import Data.Array
import Data.Maybe (maybe, catMaybes, fromJust, isJust, isNothing)
import Graphics.Gloss.Interface.Pure.Game

updateMessage :: Game -> Game
updateMessage game = game { message = "Player1: " ++ (show (player1Score game)) ++ ", Player2: " ++ (show (player2Score game))}

setToggled :: Maybe Pos -> Game -> Game
setToggled Nothing game = game { marker = (marker game) {toggled = Nothing} }
setToggled (Just pos) game = game { marker = (marker game) {toggled = Just pos} }

updNoBoxGame :: Game -> (Pos, Pos) -> Game
updNoBoxGame game (p0, p1) =
    game { marker = (marker game) { toggled = Nothing }
         , gamePlayer = opposite (gamePlayer game)
         , possibleMoves = removeMoves (p0, p1) (possibleMoves game)
         }
         where
         opposite currPlayer = if currPlayer == Player1 then Player2 else Player1

updBoxGame :: Game -> Int -> (Pos, Pos) -> Game
updBoxGame game numBoxFormed (p0, p1) =
    game { marker = (marker game) { toggled = Nothing}
         , player1Score = if ((gamePlayer game) == Player1) then (numBoxFormed + (player1Score game)) else (player1Score game)
         , player2Score = if ((gamePlayer game) == Player2) then (numBoxFormed + (player2Score game)) else (player2Score game)
         , gameWinner = winner
         , gameState = getState
         , possibleMoves = removeMoves (p0, p1) (possibleMoves game)
         }
         where
         winner = if gamePlayer game == Player1 && ((player1Score game) + numBoxFormed) * 2 > ((n-1) * (n-1))
                   then Just Player1
                   else (if gamePlayer game == Player2 && ((player2Score game) + numBoxFormed) * 2 > ((n-1) * (n-1))
                            then Just Player2
                            else Nothing)
         getState = if winner == Nothing then Running else GameOver winner

updateBoxNotFormedGame :: Game -> (Pos, Pos) -> Game
updateBoxNotFormedGame game (p0, p1) =
      if gameMode == humanMode || gamePlayer game == Player2
        then updNoBoxGame game (p0, p1)
        else updateMessage (drawLine (playComputer (updNoBoxGame game (p0, p1))))

updateBoxFormedGame :: Game -> Int -> (Pos, Pos) -> Game
updateBoxFormedGame game numBoxFormed (p0, p1) =
  if gameMode /= humanMode && gamePlayer game == Player2
    then (if gameState newGame /= Running
            then newGame
            else updateMessage (drawLine (playComputer newGame)))
    else updBoxGame game numBoxFormed (p0, p1)
    where
        newGame = updBoxGame game numBoxFormed (p0, p1)

drawLine :: Game -> Game
drawLine game
    | (isNothing newBoard)  = game
    | otherwise             = if numBoxFormed /= 0
                                then updateBoxFormedGame newGame numBoxFormed (p0, p1)
                                else updateBoxNotFormedGame newGame (p0, p1)
    where
        board       = gameBoard game
        newBoard    = stepBoard board
        newGame     = game {gameBoard = fromJust newBoard}
        (p0, p1) = (fromJust $ toggled $ marker game, position $ marker game)
        isValid =   (   (abs (fst p0 - fst p1) == 1 && snd p0 == snd p1)
                        || (abs (snd p0 - snd p1) == 1 && fst p0 == fst p1)
                    )
                    &&  (board ! (p0, p1) == Nothing)
        numBoxFormed = if (abs (fst p0 - fst p1) == 1 && snd p0 == snd p1)
                        then isLeftBoxFormed + isRightBoxFormed
                        else isTopBoxFormed  + isBottomBoxFormed
        isLeftBoxFormed     = if (      snd p0 > 0
                                        && (board ! ((fst p0, snd p0 - 1), (fst p1, snd p1 - 1)) /= Nothing)
                                        && (board ! ((fst p0, snd p0), (fst p0, snd p0 - 1)) /= Nothing)
                                        && (board ! ((fst p1, snd p1), (fst p1, snd p1 - 1)) /= Nothing)
                                 )      then 1
                                        else 0
        isRightBoxFormed    = if (      snd p0 < (n - 1)
                                        && (board ! ((fst p0, snd p0 + 1), (fst p1, snd p1 + 1)) /= Nothing)
                                        && (board ! ((fst p0, snd p0), (fst p0, snd p0 + 1)) /= Nothing)
                                        && (board ! ((fst p1, snd p1), (fst p1, snd p1 + 1)) /= Nothing)
                                 )      then 1
                                        else 0
        isTopBoxFormed      = if (      fst p0 < (n - 1)
                                        && (board ! ((fst p0 + 1, snd p0), (fst p1 + 1, snd p1)) /= Nothing)
                                        && (board ! ((fst p0, snd p0), (fst p0 + 1, snd p0)) /= Nothing)
                                        && (board ! ((fst p1, snd p1), (fst p1 + 1, snd p1)) /= Nothing)
                                 )      then 1
                                        else 0
        isBottomBoxFormed   = if (      fst p0 > 0
                                        && (board ! ((fst p0 - 1, snd p0), (fst p1 - 1, snd p1)) /= Nothing)
                                        && (board ! ((fst p0, snd p0), (fst p0 - 1, snd p0)) /= Nothing)
                                        && (board ! ((fst p1, snd p1), (fst p1 - 1, snd p1)) /= Nothing)
                                 )      then 1
                                        else 0

        stepBoard :: Board -> Maybe Board
        stepBoard board
            | isValid   = Just $ board // [((p0, p1), Just (gamePlayer game)), ((p1, p0), Just (gamePlayer game))]
            | otherwise = Nothing

removeMoves :: (Pos, Pos) -> Possibilities -> Possibilities
removeMoves (p0, p1) setofMoves = [(a, b) | (a, b) <- setofMoves, (a /= p0 || b /= p1) && (a /= p1 || b /= p0)]

movePos :: Pos -> SpecialKey -> Pos
movePos (a,b) KeyLeft  = if (b > 0) then (a,b-1) else (a,b)
movePos (a,b) KeyRight = if (b < (n - 1)) then (a,b+1) else (a,b)
movePos (a,b) KeyDown  = if (a > 0) then (a-1,b) else (a,b)
movePos (a,b) KeyUp    = if (a < (n - 1)) then (a+1,b) else (a,b)
movePos p     _        = p

setMarker :: Pos -> Game -> Game
setMarker pos game = game { marker = (marker game) {position = pos} }

transformGame :: Event -> Game -> Game
transformGame (EventKey (SpecialKey KeyUp)    Down _ _) game = setMarker (movePos (position . marker $ game) KeyUp) game
transformGame (EventKey (SpecialKey KeyDown)  Down _ _) game = setMarker (movePos (position . marker $ game) KeyDown) game
transformGame (EventKey (SpecialKey KeyLeft)  Down _ _) game = setMarker (movePos (position . marker $ game) KeyLeft) game
transformGame (EventKey (SpecialKey KeyRight) Down _ _) game = setMarker (movePos (position . marker $ game) KeyRight) game
transformGame (EventKey (SpecialKey KeySpace) Down _ _) game
    | isNothing tog         = setToggled (Just pos) game
    | pos == fromJust tog   = setToggled Nothing game
    | otherwise             = updateMessage (drawLine game)
    where
        tog = toggled . marker $ game
        pos = position . marker $ game
transformGame _ game = game
