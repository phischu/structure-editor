module Main where


import Graphics.Gloss.Interface.Pure.Game

type Extent = (Float,Float)

data GUI a =
    Button Point Extent Picture (Maybe a) |
    Two (GUI a) (GUI a)

render :: GUI a -> Picture
render (Button position _ pict _) = uncurry translate position pict
render (Two gui1 gui2) = pictures [render gui1,render gui2]

handle :: Event -> GUI a -> Maybe a
handle (EventKey (MouseButton LeftButton) Up _ clickposition) (Button buttonposition buttonsize _ maybevalue)
    | inside clickposition buttonposition buttonsize = maybevalue
    | otherwise = Nothing 
handle _ _ = Nothing

inside :: Point -> Point -> Extent -> Bool
inside (c1,c2) (b1,b2) (w,h) = and [
    c1 > b1 - whalf,
    c1 < b1 + whalf,
    c2 > b2 - hhalf,
    c2 < b2 + hhalf] where
        whalf = 0.5 * w
        hhalf = 0.5 * h

button :: String -> Color -> Point -> Extent -> Maybe a -> GUI a
button caption col position extent maybevalue =
    Button position extent pict maybevalue where
        pict = pictures [
            color col (uncurry rectangleSolid extent),
            translate (-0.5 * w + 5) (0.5 * h - 25) (scale 0.2 0.2 (text caption))]
        (w,h) = extent

also :: GUI a -> GUI a -> GUI a
also = Two

data Remou a =
    RemouCircle (Number a) a |
    RemouTranslate (Number a) (Number a) (Remou a) a

data Number a = Number Integer a

renderRemou :: Remou a -> Picture
renderRemou (RemouCircle (Number radius _) _) = circle (fromIntegral radius)
renderRemou (RemouTranslate (Number t1 _) (Number t2 _) remou _) =
    translate (fromIntegral t1) (fromIntegral t2) (renderRemou remou)

type Width = Float

guiRemou :: Width -> Remou a -> GUI a
guiRemou w (RemouCircle radius _) = nest w "Circle" [guiNumber w radius]
guiRemou w (RemouTranslate t1 t2 remou _) = nest w "Translate" [
    guiNumber w t1,
    guiNumber w t2,
    guiRemou w remou]

guiNumber :: Width -> Number a -> GUI a
guiNumber w (Number n _) = nest w (show n) []

nest :: Width -> String -> [GUI a] -> GUI a
nest = undefined


testgui :: GUI Integer
testgui =
    button "Push" red (100,100) (200,200) Nothing `also`
    button "Hehe" green (90,90) (100,100) Nothing

testast :: Remou Integer
testast =
    RemouTranslate (Number 40 0) (Number 60 1) (RemouCircle (Number 20 4) 3) 2

main :: IO ()
main = play
    (InWindow "stuff" (1000,500) (100,100))
    white
    40
    testast
    renderRemou
    (const id)
    (const id)


