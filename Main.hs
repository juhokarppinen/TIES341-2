module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data World = World
    {
        ballInstance :: Object
    } deriving Show


data Object = Object
    {
        loc :: (Float,Float),
        vel :: (Float,Float),
        acc :: (Float,Float),
        pic :: Picture
    } deriving Show


initialState :: World
initialState = World (
    Object 
        (0,0) 
        (0,0) 
        (0,0) 
        (circleSolid 10))


render :: World -> Picture
render w = let ball = ballInstance w in
    uncurry translate (loc ball) $ pic ball 


handleInput :: Event -> World -> World
handleInput e w = w 


iterateState :: Float -> World -> World
iterateState f w = w     


main :: IO ()
main = play
    (InWindow "Bouncing Ball" (640,640) (10,10))
    white
    60
    initialState
    render
    handleInput
    iterateState
