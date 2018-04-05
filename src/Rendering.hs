module Rendering where

import Data.Array

import Graphics.Gloss

import Game

boardAsRunningPicture board = Blank

outcomeColor (Just Player1) = makeColorI 255 50 50 255     -- red
outcomeColor (Just Player2) = makeColorI 50 100 255 255    -- blue
outcomeColor Nothing = greyN 0.5

translateDash picture dot1Row dot1Column ex ey =
    translate x y picture
    where x = fromIntegral dot1Column * boxWidth + boxWidth * ex
          y = fromIntegral dot1Row * boxHeight + boxHeight * ey

snapPictureToDash picture ((dot1Row, dot1Column), (dot2Row, dot2Column)) =
    if dot1Row == dot2Row
        then translateDash picture dot1Row dot1Column 0.5 0
        else translateDash (rotate 90 picture) dot1Row dot1Column 0 0.5

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
player2Dashes board = dashesOfBoard board (Just Player1) linePicture

drawRowDots rowNum = concatMap (\i -> [ translate (i * boxWidth) (rowNum * boxHeight) (thickCircle 1.0 2.0)
                              ])
                    [0 .. fromIntegral (n - 1)]

dotsOfBoard :: Picture
dotsOfBoard =
    pictures
    $ concatMap (\i -> drawRowDots i)
     [0 .. fromIntegral (n - 1)]

boardAsPicture board =
    pictures [ dotsOfBoard,
               player1Dashes board,
               player2Dashes board
             ]

boardAsGameOverPicture winner board =
    color (outcomeColor winner) (boardAsPicture board)

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5) + boxWidth)
                               (fromIntegral screenHeight * (-0.5) + boxHeight)
                               frame
    where frame = case gameState game of
                    Running -> boardAsRunningPicture (gameBoard game)
                    GameOver winner -> boardAsGameOverPicture winner (gameBoard game)