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

pMessage game = color white
                $ translate ((boxWidth game)* (-1)) ((boxWidth game)* (-1)) $ scale 0.15 0.15
                $ text $ (message game)

translatePos :: Game -> Pos -> (Float, Float)
translatePos game (r, c) = ((boxWidth game) *  fromIntegral r, (boxWidth game) * fromIntegral c)

drawMarker :: Game -> Picture
drawMarker game =
    let
        (tx, ty) = translatePos game $ position (marker game)
        markerPic = color tGreen $ translate ty tx $ thickCircle 15 6
    in  markerPic

drawToggled :: Game -> Picture
drawToggled game =
    let tog = toggled (marker game)
        (tx, ty) = translatePos game $ maybe (0, 0) id tog
        toggler = color tYellow $ translate ty tx $ thickCircle 15 6
    in  if (isNothing tog)
            then Blank
            else toggler

boardAsRunningPicture :: Game -> Picture
boardAsRunningPicture game =
    pictures [ color boardDotColor (dotsOfBoard game)
             , color player1Color $ player1Dashes game
             , color player2Color $ player2Dashes game
             , drawMarker game
             , drawToggled game
             , pMessage game
             ]

outcomeColor (Just Player1) = makeColorI 255 50 50 255     -- red
outcomeColor (Just Player2) = makeColorI 50 100 255 255    -- blue
outcomeColor Nothing = greyN 0.5

translateDash :: Game -> Picture -> Int -> Int -> Float -> Float -> Picture
translateDash game picture dot1Row dot1Column ex ey =
    translate x y picture
    where x = fromIntegral dot1Column * (boxWidth game) + (boxWidth game) * ex
          y = fromIntegral dot1Row * (boxHeight game) + (boxHeight game) * ey

snapPictureToDash :: Game -> Picture -> ((Int, Int), (Int, Int)) -> Picture
snapPictureToDash game picture ((dot1Row, dot1Column), (dot2Row, dot2Column)) =
    if dot1Row == dot2Row
        then (if dot1Column > dot2Column
                then translateDash game picture dot1Row dot2Column 0.5 0
                else translateDash game picture dot1Row dot1Column 0.5 0)
        else (if dot1Row < dot2Row
                then translateDash game (rotate 90 picture) dot1Row dot1Column 0 0.5
                else translateDash game (rotate 90 picture) dot2Row dot1Column 0 0.5)

linePicture :: Game -> Picture
linePicture game = pictures [rectangleSolid (boxWidth game) 5.0]

dashesOfBoard :: Game -> Dash -> Picture -> Picture
dashesOfBoard game dash dashPicture =
    pictures
    $ map (snapPictureToDash game dashPicture . fst)
    $ filter (\(_, e) -> e == dash)
    $ assocs (gameBoard game)

player1Dashes :: Game -> Picture
player1Dashes game = dashesOfBoard game (Just Player1) (linePicture game)

player2Dashes :: Game -> Picture
player2Dashes game = dashesOfBoard game (Just Player2) (linePicture game)

drawRowDots rowNum game =
    concatMap (\i -> [ translate (i * (boxWidth game)) (rowNum * (boxHeight game)) (thickCircle 1.0 2.0)]) [0 .. fromIntegral (n - 1)]
    where n = numDots game

dotsOfBoard :: Game -> Picture
dotsOfBoard game =
    pictures
    $ concatMap (\i -> drawRowDots i game)
     [0 .. fromIntegral (n - 1)]
    where n = numDots game

boardAsPicture :: Game -> Picture
boardAsPicture game =
    pictures [ dotsOfBoard game,
               player1Dashes game,
               player2Dashes game,
               drawMarker game,
               drawToggled game,
               pMessage game
             ]

boardAsGameOverPicture :: Game -> Picture
boardAsGameOverPicture game =
    color (outcomeColor (gameWinner game)) (boardAsPicture game)

gameAsPicture :: Game -> Picture
gameAsPicture game =
    translate (fromIntegral screenWidth * (-0.5) + (boxWidth game))
              (fromIntegral screenHeight * (-0.5) + (boxHeight game))
              frame
    where frame = case gameState game of
                    Running -> boardAsRunningPicture game
                    GameOver winner -> boardAsGameOverPicture game
