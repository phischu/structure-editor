module Main where


import Graphics.Gloss.Interface.Pure.Game

import Control.Monad (msum)

type Extent = (Float,Float)

data GUI a =
    Pic Picture |
    Click Extent a |
    Position Point (GUI a) |
    Elements [GUI a]

render :: GUI a -> Picture
render (Pic pic) = pic
render (Position (p1,p2) gui) = translate p1 (negate p2) (render gui)
render (Elements elements) = pictures (map render elements)
render _ = blank

data MouseDown = MouseDown Point

handle :: Event -> GUI a -> Maybe a
handle (EventKey (MouseButton LeftButton) Down _ m) = handleMouseDown (MouseDown m)
handle _ = const Nothing

handleMouseDown :: MouseDown -> GUI a -> Maybe a
handleMouseDown (MouseDown m) (Click extent a)
    | inside m extent = Just a
    | otherwise = Nothing
handleMouseDown (MouseDown (m1,m2)) (Position (p1,p2) gui) = handleMouseDown (MouseDown (m1 - p1,m2 - p2)) gui
handleMouseDown mousedown (Elements elements) = msum (reverse (map (handleMouseDown mousedown) elements))

inside :: Point -> Extent -> Bool
inside (c1,c2) (w,h) = and [
    c1 > 0,
    c1 < w,
    c2 > 0,
    c2 < h]

data Remou a =
    RemouCircle (Number a) a |
    RemouTranslate (Number a) (Number a) (Remou a) a

data Number a = Number Integer a

data Nest a = Nest String a [Nest a]

renderRemou :: Remou a -> Picture
renderRemou (RemouCircle (Number radius _) _) = circle (fromIntegral radius)
renderRemou (RemouTranslate (Number t1 _) (Number t2 _) remou _) =
    translate (fromIntegral t1) (fromIntegral t2) (renderRemou remou)

type Width = Float

nestRemou :: Remou a -> Nest a
nestRemou (RemouCircle radius a) = Nest "Circle" a [nestNumber radius]
nestRemou (RemouTranslate t1 t2 remou a) = Nest "Translate" a [
    nestNumber t1,
    nestNumber t2,
    nestRemou remou]

nestNumber :: Number a -> Nest a
nestNumber (Number n a) = Nest (show n) a []

guiNest :: Width -> Nest a -> GUI a
guiNest w (Nest caption a nests) = Elements [Pic pic,Click (w,h) a,Position (25,25) (stack (w-25) nests)] where
    pic = pictures [
        translate (0.5 * w) (-0.5 * h) (rectangleWire w h),
        translate 5 (-20) (scale 0.15 0.15 (text caption))]
    h = 25 + sum (map height nests)

stack :: Width -> [Nest a] -> GUI a
stack _ [] = Elements []
stack w (nest:nests) = Elements [guiNest w nest,Position (0,height nest) (stack w nests)]

height :: Nest a -> Float
height (Nest _ _ nests) = 25 + sum (map height nests)

testgui :: GUI Integer
testgui = guiNest 300 (nestRemou testast)

testast :: Remou Integer
testast =
    RemouTranslate (Number 40 0) (Number 60 1) (RemouCircle (Number 20 4) 3) 2

main :: IO ()
main = play
    (InWindow "stuff" (1000,500) (100,100))
    white
    40
    testgui
    render
    (const id)
    (const id)


