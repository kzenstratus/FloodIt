module Move where

import Board exposing (..)
import Array exposing (Array, fromList)

-------------------------------------------------
----- HELPER FUNCTIONS FOR CHANGING STATE -------
-------------------------------------------------
-- Color dictated by the (0,0) square
currColor : Board -> Int
currColor b = 
    let pixel = arrayGet 0 <| arrayGet 0 b
    in  pixel.shade


-- reduces a list of possible next pixels if pixel is 
-- out of bounds was the parent or is a different color
trimCoord : Int -> Int -> Pixel -> List Point-> Board -> List Pixel
trimCoord baseColor size pixel possible b=
    case possible of
        [] -> []
        p::ps ->
            if p == pixel.loc then trimCoord baseColor size pixel ps b
            else 
            if p.x >= size || p.x < 0 || p.y >= size || p.y < 0 then
                trimCoord baseColor size pixel ps b
            else 

            let currPix = pntToPix b p
                in 
            if currPix.shade /= baseColor then trimCoord baseColor size pixel ps b
            else 

            currPix :: (trimCoord baseColor size pixel ps b)

pntToPix : Board -> Point  -> Pixel
pntToPix b pnt =  arrayGet pnt.x (arrayGet pnt.y b)

-- Gives a list of pixel coordinates that have the same color
scan : Int -> Int -> Pixel -> Pixel -> Board -> List Pixel
scan baseColor size parent pixel b =
    let 
        up     = {x = pixel.loc.x, y = pixel.loc.y + 1}
        down   = {x = pixel.loc.x, y = pixel.loc.y - 1}
        right  = {x = pixel.loc.x + 1, y = pixel.loc.y}
        left   = {x = pixel.loc.x - 1, y = pixel.loc.y}
        total  = [up,down,right,left]
        in
    trimCoord baseColor size parent total b


insert : Pixel -> Board -> Board
insert pix b = 
    let row    = arrayGet pix.loc.y b
        newRow = Array.set pix.loc.x pix row
        in
    Array.set pix.loc.y newRow b

traverseHelper : Int -> Int -> Point -> List Pixel -> Board -> Board 
traverseHelper baseColor newColor parent currL b =
    case currL of
        []    -> b
        p::ps -> 
            let newB = traverse baseColor newColor parent p.loc b
                in
            traverseHelper baseColor newColor p.loc ps newB

-- Takes a current point, scans the rest, then recolors current point
traverse : Int -> Int -> Point -> Point -> Board -> Board
traverse baseColor newColor parent curr b = 
    let size     = Array.length b
        parPix   =  pntToPix b parent
        currPix  =  pntToPix b curr
        next     = scan baseColor size parPix currPix b
        newPix   = {shade = newColor, loc = curr}
        newB     = insert newPix b
        in
    case next of
        []    -> newB
        ps -> traverseHelper baseColor newColor curr ps newB


-- Update method which changes the colors on the board
-- Floodfill changes the color of the board based on the current color
-- Current color is an Int
-- 1. Scan, 2. Change current color 3. Recurse

------------------------------------------
--------UPSTATE FUNCTION -----------------
------------------------------------------
floodFill : Int -> Board -> Board
floodFill newColor b = 
    let start      = {x = 0,y = 0}
        baseColor' = pntToPix b start
        baseColor  = baseColor'.shade
        in
    traverse baseColor newColor start start b


-------------------------------------------
----------Game Components -----------------
-------------------------------------------

-- Checks to see if the board is all the same color
didWin : Board -> Bool
didWin b =
    let c = currColor b
        size = (Array.length b) - 1
        iterate b2 negIndex =
            if negIndex > 0 then 
                if didWinHelper size c <| arrayGet negIndex b2 then 
                    iterate b2 (negIndex-1)
                else False
            else if negIndex == 0 then
                if didWinHelper size c <| arrayGet negIndex b2 then True
                else False
            else Debug.crash ("shouldn't get to negative index")
        in
    iterate b size 

-- Traverses down one row and determines if full
didWinHelper : Int -> Int -> Array Pixel -> Bool
didWinHelper size color pixels= 
    if size >= 0 then 
        case (Array.toList pixels) of
            []    -> True
            p::ps -> if color /= p.shade then False
                     else didWinHelper (size - 1) color (Array.fromList ps) 
    else True