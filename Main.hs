module Main where


import Graphics.Gloss.Interface.Pure.Game

type Extent = (Float,Float)

data GUI a =
    Empty |
    Pic Picture (GUI a) |
    Click Extent a (GUI a) |
    Position Point (GUI a) |
    Elements [GUI a]

render :: GUI a -> Picture
render (Pic pic gui) = pictures [pic,render gui]
render (Position (p1,p2) gui) = translate p1 (negate p2) (render gui)
render (Elements elements) = pictures (map render elements)
render _ = blank

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

data Nest = Nest String [Nest]

renderRemou :: Remou a -> Picture
renderRemou (RemouCircle (Number radius _) _) = circle (fromIntegral radius)
renderRemou (RemouTranslate (Number t1 _) (Number t2 _) remou _) =
    translate (fromIntegral t1) (fromIntegral t2) (renderRemou remou)

type Width = Float

nestRemou :: Remou a -> Nest
nestRemou (RemouCircle radius _) = Nest "Circle" [nestNumber radius]
nestRemou (RemouTranslate t1 t2 remou _) = Nest "Translate" [
    nestNumber t1,
    nestNumber t2,
    nestRemou remou]

nestNumber :: Number a -> Nest
nestNumber (Number n _) = Nest (show n) []

guiNest :: Width -> Nest -> GUI a
guiNest w (Nest caption nests) = Elements [Pic pic Empty,Position (25,25) (stack (w-25) nests)] where
    pic = pictures [
        translate (0.5 * w) (-0.5 * h) (rectangleWire w h),
        translate 0 (-25) (scale 0.25 0.25 (text caption))]
    h = 25 + sum (map height nests)

stack :: Width -> [Nest] -> GUI a
stack _ [] = Empty
stack w (nest:nests) = Elements [guiNest w nest,Position (0,height nest) (stack w nests)]

height :: Nest -> Float
height (Nest _ nests) = 25 + sum (map height nests)

testgui :: GUI Integer
testgui = guiNest 300 (Nest "Hallo" [Nest "Wordl" [],Nest "hihi" []])

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


