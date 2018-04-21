module Rendering where

import Data.Array
import Data.Maybe (maybe, catMaybes, fromJust, isJust, isNothing)

import Graphics.Gloss

import Game

boardDotColor = makeColorI 255 255 255 255               -- white
player1Color = makeColorI 255 50 50 255                  -- red
player2Color = makeColorI 50 100 255 255                 -- blue
tieColor = greyN 0.5

tYellow = makeColorI 255 0 255 255
tGreen  = makeColorI 0 255 0 255

translatePos :: Pos -> (Float, Float)
translatePos (r, c) = (boxWidth *  fromIntegral r, boxWidth * fromIntegral c)

drawMarker :: Marker -> Picture
drawMarker m =  let (tx, ty) = translatePos $ position m
                    marker = color tGreen $ translate ty tx $ thickCircle 15 6
                in  marker

drawToggled :: Marker -> Picture
drawToggled m = let tog = toggled m
                    (tx, ty) = translatePos $ maybe (0, 0) id tog
                    toggler = color tYellow $ translate ty tx $ thickCircle 15 6
                in  if (isNothing tog)  then Blank
                                        else toggler

boardAsRunningPicture :: Game -> Picture
boardAsRunningPicture game =
    pictures [ color boardDotColor dotsOfBoard
             , color player1Color $ player1Dashes (gameBoard game)
             , color player2Color $ player2Dashes (gameBoard game)
             , drawMarker (marker game)
             , drawToggled (marker game)
             ]

outcomeColor (Just Player1) = makeColorI 255 50 50 255     -- red
outcomeColor (Just Player2) = makeColorI 50 100 255 255    -- blue
outcomeColor Nothing = greyN 0.5

translateDash :: Picture -> Int -> Int -> Float -> Float -> Picture
translateDash picture dot1Row dot1Column ex ey =
    translate x y picture
    where x = fromIntegral dot1Column * boxWidth + boxWidth * ex
          y = fromIntegral dot1Row * boxHeight + boxHeight * ey

snapPictureToDash :: Picture -> ((Int, Int), (Int, Int)) -> Picture
snapPictureToDash picture ((dot1Row, dot1Column), (dot2Row, dot2Column)) =
    if dot1Row == dot2Row
        then (if dot1Column > dot2Column
                then translateDash picture dot1Row dot2Column 0.5 0
                else translateDash picture dot1Row dot1Column 0.5 0)
        else (if dot1Row < dot2Row
                then translateDash (rotate 90 picture) dot1Row dot1Column 0 0.5
                else translateDash (rotate 90 picture) dot2Row dot1Column 0 0.5)

linePicture :: Picture
linePicture = pictures [rectangleSolid boxWidth 5.0]

dashesOfBoard :: Board -> Dash -> Picture -> Picture
dashesOfBoard board dash dashPicture =
    pictures
    $ map (snapPictureToDash dashPicture . fst)
    $ filter (\(_, e) -> e == dash)
    $ assocs board

player1Dashes :: Board -> Picture
player1Dashes board = dashesOfBoard board (Just Player1) linePicture

player2Dashes :: Board -> Picture
player2Dashes board = dashesOfBoard board (Just Player2) linePicture

drawRowDots rowNum = concatMap (\i -> [ translate (i * boxWidth) (rowNum * boxHeight) (thickCircle 1.0 2.0)
                              ])
                    [0 .. fromIntegral (n - 1)]

dotsOfBoard :: Picture
dotsOfBoard =
    pictures
    $ concatMap (\i -> drawRowDots i)
     [0 .. fromIntegral (n - 1)]

boardAsPicture :: Game -> Picture
boardAsPicture game =
    pictures [ dotsOfBoard,
               player1Dashes (gameBoard game),
               player2Dashes (gameBoard game),
               drawMarker (marker game),
               drawToggled (marker game)
             ]

boardAsGameOverPicture :: Dash -> Game -> Picture
boardAsGameOverPicture winner game =
    color (outcomeColor winner) (boardAsPicture game)

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5) + boxWidth)
                               (fromIntegral screenHeight * (-0.5) + boxHeight)
                               frame
    where frame = case gameState game of
                    Running -> boardAsRunningPicture game
                    GameOver winner -> boardAsGameOverPicture winner game
