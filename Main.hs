module Main where


import Graphics.Gloss.Interface.Pure.Display

type Extent = (Float,Float)

data GUI a =
    Button Point Extent Picture (Maybe a) |
    Two (GUI a) (GUI a)

render :: GUI a -> Picture
render (Button position _ pict _) = uncurry translate position pict
render (Two gui1 gui2) = pictures [render gui1,render gui2]

button :: String -> Color -> Point -> Extent -> Maybe a -> GUI a
button caption col position extent maybevalue =
    Button position extent pict maybevalue where
        pict = (color col (uncurry rectangleSolid extent))

testgui :: GUI Integer
testgui = button "Push" red (100,100) (200,200) Nothing

main :: IO ()
main = display (InWindow "stuff" (600,600) (300,300)) white (render testgui)