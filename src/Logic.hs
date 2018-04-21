module Logic where

import Game
import Rendering
import Data.Array
import Data.Maybe (maybe, catMaybes, fromJust, isJust, isNothing)
import Graphics.Gloss.Interface.Pure.Game

setToggled :: Maybe Pos -> Game -> Game
setToggled Nothing game = game { marker = (marker game) {toggled = Nothing} }
setToggled (Just pos) game = game { marker = (marker game) {toggled = Just pos} }

updateGame :: Game -> Game
updateGame game = game { marker = (marker game) { toggled = Nothing }
                       , gamePlayer = opposite (gamePlayer game) }
                       where
                           opposite currPlayer = if currPlayer == Player1 then Player2 else Player1

drawLine :: Game -> Game
drawLine game
    | (isNothing newBoard)  = game
    | otherwise             = updateGame newGame
    where
        board       = gameBoard game
        newBoard    = stepBoard board
        newGame     = game {gameBoard = fromJust newBoard}

        stepBoard :: Board -> Maybe Board
        stepBoard board
            | isValid   = Just $ board // [((p0, p1), Just (gamePlayer game))]
            | otherwise = Nothing

        (p0, p1) = (fromJust $ toggled $ marker game, position $ marker game)
        isValid =   (   (abs (fst p0 - fst p1) == 1 && snd p0 == snd p1)
                        || (abs (snd p0 - snd p1) == 1 && fst p0 == fst p1)
                    )
                    &&  (board ! (p0, p1) == Nothing)


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