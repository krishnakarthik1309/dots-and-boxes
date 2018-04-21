module Logic where

import Game
import Rendering
import Data.Array
import Data.Maybe (maybe, catMaybes, fromJust, isJust, isNothing)
import Graphics.Gloss.Interface.Pure.Game

setToggled :: Maybe Pos -> Game -> Game
setToggled Nothing game = game { marker = (marker game) {toggled = Nothing} }
setToggled (Just pos) game = game { marker = (marker game) {toggled = Just pos} }

updateBoxNotFormedGame :: Game -> Game
updateBoxNotFormedGame game = game { marker = (marker game) { toggled = Nothing }
                                   , gamePlayer = opposite (gamePlayer game)}
                              where
                                opposite currPlayer = if currPlayer == Player1 then Player2 else Player1

updateBoxFormedGame :: Game -> Game
updateBoxFormedGame game =  game { marker = (marker game) { toggled = Nothing }
                                 , player1Score = if ((gamePlayer game) == Player1) then (1 + (player1Score game)) else (player1Score game)
                                 , player2Score = if ((gamePlayer game) == Player2) then (1 + (player2Score game)) else (player2Score game)
                                 , gameWinner = winner
                                 , gameState = getState }
                            where
                                winner = if gamePlayer game == Player1 && ((player1Score game) + 1) * 2 > ((n-1) * (n-1))
                                            then Just Player1
                                            else (if gamePlayer game == Player2 && ((player2Score game) + 1) * 2 > ((n-1) * (n-1))
                                                    then Just Player2
                                                    else Nothing)
                                getState = if winner == Nothing then Running else GameOver winner

drawLine :: Game -> Game
drawLine game
    | (isNothing newBoard)  = game
    | otherwise             = if isBoxFormed
                                then updateBoxFormedGame newGame
                                else updateBoxNotFormedGame newGame
    where
        board       = gameBoard game
        newBoard    = stepBoard board
        newGame     = game {gameBoard = fromJust newBoard}
        (p0, p1) = (fromJust $ toggled $ marker game, position $ marker game)
        isValid =   (   (abs (fst p0 - fst p1) == 1 && snd p0 == snd p1)
                        || (abs (snd p0 - snd p1) == 1 && fst p0 == fst p1)
                    )
                    &&  (board ! (p0, p1) == Nothing)
        isBoxFormed = if (abs (fst p0 - fst p1) == 1 && snd p0 == snd p1)
                        then isLeftBoxFormed || isRightBoxFormed
                        else isTopBoxFormed  || isBottomBoxFormed
        isLeftBoxFormed     = if snd p0 > 0
                                then    (board ! ((fst p0, snd p0 - 1), (fst p1, snd p1 - 1)) /= Nothing)
                                        && (board ! ((fst p0, snd p0), (fst p0, snd p0 - 1)) /= Nothing)
                                        && (board ! ((fst p1, snd p1), (fst p1, snd p1 - 1)) /= Nothing)
                                else False
        isRightBoxFormed    = if snd p0 < (n - 1)
                                then    (board ! ((fst p0, snd p0 + 1), (fst p1, snd p1 + 1)) /= Nothing)
                                        && (board ! ((fst p0, snd p0), (fst p0, snd p0 + 1)) /= Nothing)
                                        && (board ! ((fst p1, snd p1), (fst p1, snd p1 + 1)) /= Nothing)
                                else False
        isTopBoxFormed      = if fst p0 < (n - 1)
                                then    (board ! ((fst p0 + 1, snd p0), (fst p1 + 1, snd p1)) /= Nothing)
                                        && (board ! ((fst p0, snd p0), (fst p0 + 1, snd p0)) /= Nothing)
                                        && (board ! ((fst p1, snd p1), (fst p1 + 1, snd p1)) /= Nothing)
                                else False
        isBottomBoxFormed   = if fst p0 > 0
                                then    (board ! ((fst p0 - 1, snd p0), (fst p1 - 1, snd p1)) /= Nothing)
                                        && (board ! ((fst p0, snd p0), (fst p0 - 1, snd p0)) /= Nothing)
                                        && (board ! ((fst p1, snd p1), (fst p1 - 1, snd p1)) /= Nothing)
                                else False
        
        stepBoard :: Board -> Maybe Board
        stepBoard board
            | isValid   = Just $ board // [((p0, p1), Just (gamePlayer game)), ((p1, p0), Just (gamePlayer game))]
            | otherwise = Nothing


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
    | otherwise             = drawLine game
    where
        tog = toggled . marker $ game
        pos = position . marker $ game
transformGame _ game = game