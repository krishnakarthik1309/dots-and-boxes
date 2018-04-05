module Rendering where

import Graphics.Gloss

import Game

boardAsRunningPicture board = Blank

outcomeColor (Just Player1) = makeColorI 255 50 50 255     -- red
outcomeColor (Just Player2) = makeColorI 50 100 255 255    -- blue
outcomeColor Nothing = greyN 0.5

drawRowDots rowNum = concatMap (\i -> [ translate (i * boxWidth) (rowNum * boxHeight) (thickCircle 1.0 2.0)
                              ])
                    [0 .. fromIntegral (n - 1)]

dotsOfBoard :: Picture
dotsOfBoard =
    pictures
    $ concatMap (\i -> drawRowDots i)
     [0 .. fromIntegral (n - 1)]

-- #TODO: draw lines
boardAsPicture board =
    pictures [ dotsOfBoard
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