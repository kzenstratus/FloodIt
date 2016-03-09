module Display where

import Graphics.Collage as C
import Graphics.Element as E
import Mouse exposing (isDown)
import Basics exposing (toFloat)
import Signal exposing (Signal,foldp,map,Mailbox, mailbox)
import Window exposing (dimensions)
import Array exposing (Array, fromList)
import List

import Color exposing (..)
import Move exposing (..)
import Graphics.Input as I
import Graphics.Input.Field as F
import Text as T
import Color as Col
import Board as B
import String as S
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import FButtons as FB
import Time exposing (Time,timestamp, fps, inMilliseconds)
import Random as R

-- Sponge Bob
--http://giphy.com/gifs/news-atl-down-MTclfCr4tVgis
-- Sadness
--http://giphy.com/gifs/inside-out-gif-10tIjpzIu8fe0
-- Ravi Image
--http://people.cs.uchicago.edu/~rchugh/

----------------------------------------------------------
-------------- STATE DEFINITION --------------------------
----------------------------------------------------------
type Outcome = Win | Lose | Playing
type Action = Ravi Bool| Hue Int| Change| Level B.Difficulty | Area B.BSize | Restart Bool
type alias State = {board : B.Board, tick: Int, ravi: Bool,
                    difficulty : B.Difficulty, b_size: B.BSize,
                    reset : Bool, outcome : Outcome
                   }

initState: State
initState = {board = B.initBoard, tick = 16, ravi = False, 
            difficulty = B.Med, b_size = B.Medium,
            reset = False, outcome = Playing
            }

--------------------------------------------
----------------- BUTTONS ------------------
{- Make signals for buttons which change the board.
-}
-- Color buttons do 2 things, change color, and decrement counter
btnMailbox : Mailbox Action
btnMailbox = mailbox (Hue 0)

colorT = B.colorTheme

btnZero : E.Element
btnZero  = FB.colorBtns (Signal.message btnMailbox.address (Hue 0)) (colorT.red)
btnOne   = FB.colorBtns (Signal.message btnMailbox.address (Hue 1)) (colorT.yellow)
btnTwo   = FB.colorBtns (Signal.message btnMailbox.address (Hue 2)) (colorT.green)
btnThree = FB.colorBtns (Signal.message btnMailbox.address (Hue 3)) (colorT.blue)
btnFour  = FB.colorBtns (Signal.message btnMailbox.address (Hue 4)) (colorT.orange)
btnFive  = FB.colorBtns (Signal.message btnMailbox.address (Hue 5)) (colorT.purple)

btnRavi  = FB.raviColorBtns (Signal.message btnMailbox.address (Hue 0))

-------------------------------------------------------
----------------- Board Size Buttons ------------------
smallBtn : E.Element
smallBtn = FB.btnMkr (Signal.message btnMailbox.address (Area B.Small)) False "Small Board"

psmallBtn : E.Element
psmallBtn = FB.btnMkr (Signal.message btnMailbox.address (Area B.Small)) True "Small Board"

medBtn   : E.Element
medBtn   = FB.btnMkr (Signal.message btnMailbox.address (Area B.Medium)) False "Medium Board"

pmedBtn   : E.Element
pmedBtn   = FB.btnMkr (Signal.message btnMailbox.address (Area B.Medium)) True "Medium Board"

largeBtn : E.Element
largeBtn = FB.btnMkr (Signal.message btnMailbox.address (Area B.Large)) False "Large Board"

plargeBtn : E.Element
plargeBtn = FB.btnMkr (Signal.message btnMailbox.address (Area B.Large)) True "Large Board"

---------- DIFFICULTY BUTTONS -------------------------------------
easyBtn : E.Element
easyBtn = FB.btnMkr (Signal.message btnMailbox.address (Level B.Easy)) False "Easy Game"

peasyBtn : E.Element
peasyBtn = FB.btnMkr (Signal.message btnMailbox.address (Level B.Easy)) True "Easy Game"

medLvLBtn : E.Element
medLvLBtn = FB.btnMkr (Signal.message btnMailbox.address (Level B.Med)) False "Medium Game"

pmedLvLBtn : E.Element
pmedLvLBtn = FB.btnMkr (Signal.message btnMailbox.address (Level B.Med)) True "Medium Game"

hardBtn : E.Element
hardBtn = FB.btnMkr (Signal.message btnMailbox.address (Level B.Hard)) False "Hard Game"

phardBtn : E.Element
phardBtn = FB.btnMkr (Signal.message btnMailbox.address (Level B.Hard)) True "Hard Game"


------------------- RESTART BUTTON -------------------------------------------------
newGame : E.Element
newGame = FB.btnMkr (Signal.message btnMailbox.address (Restart True)) False "New Game"

------------------- RAVI Mode BUTTON -----------------------------------------------

-- These button colors are inverted somehow, not sure why pravi should be True True
raviGame : E.Element
raviGame = FB.btnMkr (Signal.message btnMailbox.address (Ravi False)) True "Ravi Mode"

praviGame : E.Element
praviGame = FB.btnMkr (Signal.message btnMailbox.address (Ravi True)) False "Ravi Mode"

------------------- WIN GIF --------------------------------------------------------
winGif: E.Element
winGif = E.fittedImage 400 400 "win.gif"

winText: E.Element
winText = "YOU WIN!" |> T.fromString |> T.style resultStyle|> E.centered 

loseGif: E.Element
loseGif = E.fittedImage 400 400 "lose.gif"

loseText: E.Element
loseText = "YOU LOSE!" |> T.fromString |> T.style resultStyle |> E.centered 
----------------------------------------------------------------------------------
-------- Wrap 2 Possible Results -------------------------------------------------

winAlert: E.Element
winAlert = E.flow E.down <| [winText]++[winGif] ++ [newGame] 

loseAlert: E.Element
loseAlert = E.flow E.down <| [loseText]++[loseGif] ++ [newGame] 

-----------------------------------------------------
--------- TEXT Style --------------------------------
-----------------------------------------------------
tickStyle: T.Style
tickStyle = { typeface = 
    [ "Helvetica Neue", "sans-serif" ],
    height   = Just 30,
    color    = black,
    bold     = False,
    italic   = False,
    line     = Nothing}

resultStyle: T.Style
resultStyle = { typeface = 
    [ "Helvetica Neue", "sans-serif" ],
    height   = Just 45,
    color    = black,
    bold     = True,
    italic   = False,
    line     = Nothing}

-----------------------------------------------
-------------- RAVI MODE ----------------------
-----------------------------------------------

mkRavi: Int -> C.Form
mkRavi l = (E.fittedImage l l "ravi.png") |> C.toForm

---------------------------------------------
----------------- STATE ---------------------
---------------------------------------------

getTickHelper: B.BSize -> Int
getTickHelper b_size = 
  case b_size of
    B.Small  -> 12
    B.Medium -> 16
    B.Large  -> 28

getTick: B.BSize -> B.Difficulty -> Int
getTick b_size level = 
  let size = toFloat <| getTickHelper b_size
    in
  case level of
    B.Easy -> 1.5 * size |> floor
    B.Med  -> size |> floor
    B.Hard -> (0.8 * size) |> floor

--- Every time upstate is called, the ticker decrements
--- Need case for whenever s.tick goes hits below 0

upstate : (Time,Action) ->  State -> State 
upstate (time,a) s =
  let seed         =  time |> inMilliseconds |> round |> R.initialSeed in

  case a of
    Hue c      -> 
        let t      = if (s.tick <= 0) then 0 else (s.tick-1)
            currBoard = floodFill c s.board
            result = 
              case ((s.tick == 1),(s.tick <= 0),(didWin currBoard)) of
                (True,_,False) -> Lose
                (_,True,False) -> Lose
                (_,True,True)  -> Lose
                (_,False,False) ->Playing
                (_,False,True) -> Win
          in

        {board = currBoard, tick = t, 
         ravi = s.ravi, difficulty = s.difficulty,
         b_size = s.b_size, reset = False, outcome = result
        }
        
    Area size  ->
      let newBoard = (B.createBoardWrap 0 size seed B.colorNum)
        in 
        {board = newBoard, tick = (getTickHelper size), ravi = s.ravi,
         difficulty = B.Med,
         b_size = size, reset = True, outcome = s.outcome
       }
    
    Level lvl  -> 
      let newBoard = (B.createBoardWrap 0 s.b_size seed B.colorNum)
        in 
        {board = newBoard, tick = (getTick s.b_size lvl), ravi = s.ravi,
        difficulty = lvl, b_size = s.b_size,reset = True, outcome = s.outcome
        }

    Restart _ ->
        let newBoard = (B.createBoardWrap 0 s.b_size seed B.colorNum)
            t        = getTick s.b_size s.difficulty
           in 
      {board = newBoard, tick = t, ravi = s.ravi,
         difficulty = s.difficulty, b_size = s.b_size,
         reset = True, outcome = Playing
       }

    Ravi x    ->
      let newBoard = (B.createBoardWrap 0 s.b_size seed B.colorNum)
          t        = getTick s.b_size s.difficulty
          in
      {board = newBoard, tick = t, ravi = x,
         difficulty = s.difficulty, b_size = s.b_size,
         reset = True, outcome = Playing
       }

    _     -> 
      {board = s.board, tick = s.tick, ravi = s.ravi,
         difficulty = s.difficulty, b_size = s.b_size,
         reset = True, outcome = s.outcome
       }

stateOT : Signal State
stateOT = Signal.foldp upstate initState (timestamp btnMailbox.signal)

----------------------------------------------
------------- GRAPHICAL INTERFACE ------------
{- Notes:
    create a grid of square objects.
    In the end need to create a lsit of forms
    Where each form is a Square
-}
-----------------------------------
-- Helper to toFormDot, helps change the board sizes.

pixelOffset: B.BSize -> Float
pixelOffset b_size = 
  case b_size of
    B.Small  -> 60.0
    B.Medium -> 30.0
    B.Large  -> 20.0

-- Need to change the size of the squares to keep same board size
toFormDot: Array Col.Color -> B.BSize -> Bool -> B.Pixel -> C.Form
toFormDot color_l b_size raviMode pixel =
    let color   = B.getColor color_l pixel.shade 
        offset  = (pixelOffset b_size) |> floor
        offsetF = toFloat offset
        sqPixel = 
          case (raviMode, pixel.shade) of
            (True,0) -> mkRavi offset
            (_,_)    -> C.filled color (C.square offsetF)

        in
    C.move (offsetF*(Basics.toFloat <|pixel.loc.x)-130, offsetF*(Basics.toFloat pixel.loc.y)-110) sqPixel

-- Turn everything into a List of Forms

arrToForm : Array B.Pixel -> Array Col.Color -> B.BSize -> Bool -> List C.Form
arrToForm pixels color_l b_size raviMode= 
  List.map (toFormDot color_l b_size raviMode) <| Array.toList <|pixels

-- Creates a board that can be shown on a browser
-- If we change setting but haven't hit reset yet, don't change the board!
webBoard : B.Board -> Array Col.Color -> B.BSize -> Bool -> List C.Form
webBoard b color_l b_size  raviMode= 
    let b_l = Array.toList b 
    in
    case b_l of 
        [] -> []
        row::rows -> (arrToForm row color_l b_size raviMode) ++ webBoard (Array.fromList rows) color_l b_size raviMode

-------------- Cases for setting Buttons -----------------

setSizeBtns: B.BSize -> List E.Element
setSizeBtns pressed = 
  case pressed of
    B.Small  -> [psmallBtn,medBtn,largeBtn]
    B.Medium -> [smallBtn,pmedBtn,largeBtn]
    B.Large  -> [smallBtn,medBtn,plargeBtn]

setLvlBtns: B.Difficulty -> List E.Element
setLvlBtns lvl = 
    case lvl of
      B.Easy  -> [peasyBtn,medLvLBtn,hardBtn]
      B.Med   -> [easyBtn,pmedLvLBtn,hardBtn]
      B.Hard  -> [easyBtn,medLvLBtn,phardBtn]

setRaviModeBtn: Bool -> E.Element
setRaviModeBtn raviMode = 
  case raviMode of
    True -> raviGame
    False-> praviGame

setCBtn: Bool -> E.Element
setCBtn raviMode = 
  let firstBtn = 
    case raviMode of
      True  -> btnRavi
      False -> btnZero
    in
  E.flow E.right <| [firstBtn , 
                     btnOne   , 
                     btnTwo   ,
                     btnThree ,
                     btnFour  ,
                     btnFive  ]


view : State ->  (Int,Int) -> H.Html
view s (w,h)    =       
    let counterF      = s.tick |> toString |> T.fromString |> T.style tickStyle |> E.centered
        b_size        = s.b_size
        lvl           = s.difficulty
        cBtns'        = setCBtn s.ravi

        board'    = C.collage (400) (400) <| webBoard s.board B.colorNum s.b_size s.ravi
                      

        sizeBtns' = E.flow E.down <| setSizeBtns b_size
        lvlBtns'  = E.flow E.down <| setLvlBtns lvl

        raviBtn   = H.fromElement (setRaviModeBtn s.ravi)
        board     = H.fromElement board' 
        cBtns     = H.fromElement cBtns'
        counterH  = H.fromElement counterF
        sizeBtns  = H.fromElement sizeBtns'
        lvlBtns   = H.fromElement lvlBtns'
        in
      case s.outcome of
        Win     -> 
          H.div [ HA.class "results" ]
                [ 
                  H.div[ HA.id "leftPanel" ]
                    [(H.div[HA.class "finished"]
                      [H.fromElement winText]),
                    (H.div[HA.id "success"]
                      [H.fromElement winGif]),
                    (H.div[HA.class "finished"]
                      [H.fromElement newGame])
                    ]

                ] 
        Lose    -> 
          H.div [ HA.class "results" ]
                [ 
                  H.div[ HA.id "leftPanel" ]
                    [(H.div[HA.class "finished"]
                      [H.fromElement loseText]),
                    (H.div[HA.id "success"]
                      [H.fromElement loseGif]),
                    (H.div[HA.class "finished"]
                      [H.fromElement newGame])
                    ]

                ] 
        Playing ->

          H.div
              [ HA.class "container" ]
              [ 
                H.div[ HA.id "leftPanel" ]
                  [(H.div[HA.class "board"]
                    [board]),
                  (H.div[HA.class "gamePlay"]
                  [counterH]),

                  (H.div[HA.class "gamePlay"]
                  [cBtns])

                  ],
                  H.div[ HA.id "rightPanel" ]
                  [(H.div[HA.class "settings"]
                    [sizeBtns,lvlBtns,raviBtn])
                  ]
              ]                                 



main : Signal H.Html
main =
  Signal.map2 view stateOT Window.dimensions 






