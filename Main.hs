{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Main where


import Graphics.Gloss.Interface.Pure.Game

import Control.Monad (msum)

type Extent = (Float,Float)

data GUI a =
    Pic Picture |
    Click Extent a |
    Position Point (GUI a) |
    Elements [GUI a]

deriving instance Functor GUI

data Editor = Editor Selection (Remou UID)

type Selection = UID
type UID = Integer

data EditorEvent =
    SelectionEvent UID |
    NumberEvent Integer

renderEditor :: Editor -> Picture
renderEditor editor = render (guiEditor editor)

guiEditor :: Editor -> GUI EditorEvent
guiEditor (Editor selection remou) = Position (-500,-300) (Elements [
    fmap SelectionEvent (guiNest 500 (nestRemou selection remou)),
    Position (750,250) (Pic (pictures [
        rectangleWire 500 500,
        renderRemou remou])),
    Position (0,500) (controlEditor selection remou)])

handleEditor :: Event -> Editor -> Editor
handleEditor event editor@(Editor selection remou) = case handle event (guiEditor editor) of
    Nothing -> Editor selection remou
    Just (SelectionEvent uid) -> Editor uid remou
    Just (NumberEvent n) -> Editor selection (changeNumberRemou n selection remou)

changeNumberRemou :: Integer -> Selection -> Remou UID -> Remou UID
changeNumberRemou n selection (RemouCircle num uid) = RemouCircle (changeNumberNumber n selection num) uid
changeNumberRemou n selection (RemouTranslate num1 num2 remou uid) = RemouTranslate
    (changeNumberNumber n selection num1)
    (changeNumberNumber n selection num2)
    (changeNumberRemou n selection remou)
    uid

changeNumberNumber :: Integer -> Selection -> Number UID -> Number UID
changeNumberNumber n selection (Number i uid)
    | selection == uid = Number n uid
    | otherwise = Number i uid

controlEditor :: Selection -> Remou UID -> GUI EditorEvent
controlEditor _ _ = Elements [Position (fromIntegral n * 5,0) (Elements [
    Click (5,100) (NumberEvent (n-100)),
    Pic (translate 2.5 (-50) (rectangleWire 5 100))]) | n <- [0..200]]

render :: GUI a -> Picture
render (Pic pic) = pic
render (Position (p1,p2) gui) = translate p1 (negate p2) (render gui)
render (Elements elements) = pictures (map render elements)
render _ = blank

data MouseDown = MouseDown Point

handle :: Event -> GUI a -> Maybe a
handle (EventKey (MouseButton LeftButton) Down _ (m1,m2)) = handleMouseDown (MouseDown (m1,-m2))
handle _ = const Nothing

handleMouseDown :: MouseDown -> GUI a -> Maybe a
handleMouseDown (MouseDown m) (Click extent a)
    | inside m extent = Just a
    | otherwise = Nothing
handleMouseDown (MouseDown (m1,m2)) (Position (p1,p2) gui) = handleMouseDown (MouseDown (m1 - p1,m2 - p2)) gui
handleMouseDown mousedown (Elements elements) = msum (reverse (map (handleMouseDown mousedown) elements))
handleMouseDown _ _ = Nothing

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

data Nest a = Nest Bool String a [Nest a]

renderRemou :: Remou a -> Picture
renderRemou (RemouCircle (Number radius _) _) = circle (fromIntegral radius)
renderRemou (RemouTranslate (Number t1 _) (Number t2 _) remou _) =
    translate (fromIntegral t1) (fromIntegral t2) (renderRemou remou)

type Width = Float

nestRemou :: Selection -> Remou UID -> Nest UID
nestRemou selection (RemouCircle radius uid) = Nest (selection == uid) "Circle" uid [nestNumber selection radius]
nestRemou selection (RemouTranslate t1 t2 remou uid) = Nest (selection == uid) "Translate" uid [
    nestNumber selection t1,
    nestNumber selection t2,
    nestRemou selection remou]

nestNumber :: Selection -> Number UID -> Nest UID
nestNumber selection (Number n uid) = Nest (selection == uid) (show n) uid []

guiNest :: Width -> Nest a -> GUI a
guiNest w (Nest selected caption a nests) = Elements [Pic pic,Click (w,h) a,Position (25,25) (stack (w-25) nests)] where
    pic = pictures [
        if selected
            then color (light red) (translate (0.5 * w) (-0.5 * h) (rectangleSolid w h))
            else blank,
        translate (0.5 * w) (-0.5 * h) (rectangleWire w h),
        translate 5 (-20) (scale 0.15 0.15 (text caption))]
    h = 25 + sum (map height nests)

stack :: Width -> [Nest a] -> GUI a
stack _ [] = Elements []
stack w (nest:nests) = Elements [guiNest w nest,Position (0,height nest) (stack w nests)]

height :: Nest a -> Float
height (Nest _ _ _ nests) = 25 + sum (map height nests)

testast :: Remou UID
testast =
    RemouTranslate (Number 40 0) (Number 60 1) (RemouCircle (Number 20 4) 3) 2

testeditor :: Editor
testeditor = Editor 0 testast

main :: IO ()
main = play
    (InWindow "stuff" (1000,600) (100,100))
    white
    40
    testeditor
    renderEditor
    handleEditor
    (const id)


