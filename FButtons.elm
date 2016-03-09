module FButtons where

import Signal exposing (Signal,foldp,map,Mailbox, mailbox)
import Color exposing (..)
import Graphics.Collage as C
import Graphics.Element as E
import Board as B
import Graphics.Input as I
import Text as T

--------------------------------------
----------- BUTTONS ------------------
-- Color buttons do 2 things, change color, and decrement counter

----------- Globals    ---------------

----- width and height of color buttons

colorT = B.colorTheme


mkCBtn : Color -> Float -> E.Element
mkCBtn btnCol degRotate =
    let 
        w       = 40
        h       = 40
        in
    C.collage w h 
        [ C.filled btnCol (C.rect w h) |> C.rotate (degrees degRotate)]

colorBtns : Signal.Message -> Color -> E.Element
colorBtns sig c =
  I.customButton sig (mkCBtn c 0) (mkCBtn c 30) (mkCBtn c 60)

mkRaviBtn : Float -> E.Element
mkRaviBtn deg = 
  let 
    w       = 40
    h       = 40
    ravi    = (E.fittedImage w h "ravi.png") |> C.toForm
    in
  C.collage w h
        [ ravi |> C.rotate (degrees deg)]

raviColorBtns : Signal.Message -> E.Element
raviColorBtns sig = 
  I.customButton sig (mkRaviBtn 0) (mkRaviBtn 30) (mkRaviBtn 60)

-----------------------------------------------------
------------------ General Buttons ---------------
-----------------------------------------------------
mkBtn: Color -> String -> E.Element
mkBtn c t =
    let 
        w       = 200
        h       = 60

        in
    C.collage w h 
      [ C.filled c (C.rect w h)
      --, C.outlined lineStyle (C.rect w h)
      , T.fromString t |> T.height 30 |> E.centered |> C.toForm
      ]

btnMkr : Signal.Message -> Bool -> String -> E.Element
btnMkr sig pressed str =
  if pressed then

    I.customButton sig (mkBtn colorT.purple str) (mkBtn colorT.green str) (mkBtn colorT.orange str)

  else 
    I.customButton sig (mkBtn colorT.orange str) (mkBtn colorT.green str) (mkBtn colorT.purple str)