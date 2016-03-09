module Board where
import Array exposing (Array, fromList)
import List exposing (length)
import Color as Col
import Random as R

type alias Shades = { red: Col.Color, yellow: Col.Color, green: Col.Color, blue: Col.Color, orange: Col.Color, purple: Col.Color}
type alias Shade = Int
type Links = Node2 Pixel Pixel | Node3 Pixel Pixel Pixel | Node4 Pixel Pixel Pixel Pixel

type alias Point = { x: Int, y: Int }

type alias Pixel = { shade: Int, loc: Point }

-- Use a 2d list
type alias Board = Array (Array Pixel)
type Difficulty = Easy | Med | Hard
type BSize      = Small | Medium | Large

-- Score for most connected boxes and their color
-- List of all possible colors


-- This type lets us map a number with a Shade
--------------------------------------
-------------- CONSTANTS -------------
--------------------------------------
    
colorTheme: Shades
colorTheme = {  red    = Col.rgba 255 102 102 0.7,
                yellow = Col.rgba 255 255 102 0.7, 
                green  = Col.rgba 178 255 102 0.7, 
                blue   = Col.rgba 102 255 255 0.7, 
                orange = Col.rgba 255 178 102 0.7, 
                purple = Col.rgba 102 102 255 0.7}

-- Lets us randomly select a color
colorNum : Array Col.Color
colorNum = Array.fromList <| [colorTheme.red,  colorTheme.yellow, colorTheme.green, colorTheme.blue, colorTheme.orange, colorTheme.purple]


-- Gives information on pixels that are connected to the current one

arrayGet: Int -> Array a -> a
arrayGet index arr = 
    case (Array.get index arr) of
        Nothing -> Debug.crash("Error - Can't get color, out of bounds")
        Just c  -> c

getColor : Array Col.Color -> Int -> Col.Color 
getColor shadeArray index = arrayGet index shadeArray

-- Generates a list of y coordinates
range : Int -> Int -> List Int
range num max =
    if num >= max then []
    else num :: range (num+1) max

-- max of 5x5 will be 5, so use num == max+1
oneValList : Int -> a -> Int -> List a
oneValList counter val max = 
    if counter >= (max+1) then []
    else val :: oneValList (counter+1) val max

-- Reversed here because inX is the constant
makeCoords : Int -> Int -> Point
makeCoords inX inY = {x = inY, y = inX}

makeRows : List Point -> List Int -> List Pixel
makeRows pnts shades =
    List.map2 (\color coords -> {shade = color, loc = coords}) shades pnts

-- Takes a board size and colorList returns a board, index starts at 0->6
-- Get list of shades, links, and points
-- pass to separate function which combines all lists into list of pixels
boardHelper : Int -> Int -> R.Seed -> Array Col.Color -> List (Array Pixel)
boardHelper index size seed shadeArr = 

    let 
        color_size              = (Array.length shadeArr) - 1
        -- Generate colors
        rowNumTemp1             = R.list size (R.int (0) (color_size))
        (rowNumTemp2, next_seed)= R.generate rowNumTemp1 seed
        rowNum                  = Array.fromList <| rowNumTemp2
        -- Locations below
        col_list                = range 0 size 
        l_points                = List.map (makeCoords index) col_list

        pixels                  = Array.fromList <| makeRows l_points rowNumTemp2
        in

    if index == size then (([] |> fromList) :: [])
    else [pixels] ++ (boardHelper (index + 1) size next_seed shadeArr)

createBoard : Int -> Int -> R.Seed -> Array Col.Color -> Board
createBoard index size seed shadeArr = 
    Array.fromList <| List.take size <| boardHelper index size seed shadeArr

-- Different cases for different sized boards
createBoardWrap : Int -> BSize -> R.Seed -> Array Col.Color -> Board
createBoardWrap index size seed shadeArr = 
    case size of
        Small-> createBoard index 5 seed shadeArr
        Medium -> createBoard index 10 seed shadeArr
        Large -> createBoard index 15 seed shadeArr


-------------------------------------------------
------------- Make Game Board -------------------
-------------------------------------------------
start = 0
boardSize = 10
initSeed : R.Seed
initSeed = R.initialSeed 6
--
initBoard : Board
initBoard = createBoardWrap start Medium initSeed colorNum






