module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- Data type contains the ball's location and velocity.
data World = World
    {
        loc :: Point,
        vel :: Vector
    } deriving Show


gravity :: Vector
gravity = (0, 0)

damping :: Float
damping = 0.95

ballPic :: Picture
ballPic = circleSolid 25

-- The initial state of the world.
initialState :: World
initialState = World (0,100) (0,0)


-- Translate the ball according to its location.
render :: World -> Picture
render w = uncurry translate (loc w) $ ballPic


-- Add velocity to the ball based on keyboard input.
handleInput :: Event -> World -> World
handleInput event world = World (loc world) newVel where
    newVel = vel world .+ (direction .* impulse) where
        (direction, impulse) = case event of
            EventKey (SpecialKey KeyUp) _ _ _ -> ((0,1),1)
            EventKey (SpecialKey KeyDown) _ _ _ -> ((0,-1),1)
            EventKey (SpecialKey KeyLeft) _ _ _ -> ((-1,0),1)
            EventKey (SpecialKey KeyRight) _ _ _ -> ((1,0),1)
            otherwise -> ((0,0),0)


-- Update the ball's velocity and location based on its acceleration.
moveBall :: Float -> World -> World
moveBall seconds world = World (loc world .+ newVel) newVel where 
    newVel
        | x <= leftBoundary || x >= rightBoundary = negX v
        | y <= bottomBoundary || y >= topBoundary = negY v
        | otherwise = v
        where
            x = fst (loc world)
            y = snd (loc world)
            v = ((vel world) .+ gravity) .* damping


-- Operators for vector math.

(.+) :: Vector -> Vector -> Vector
(ax, ay) .+ (bx, by) = (ax + bx, ay + by)

(.*) :: Vector -> Float -> Vector
(x, y) .* s = (x * s, y * s)

negX :: Vector -> Vector
negX (x,y) = (-x,y)

negY :: Vector -> Vector
negY (x,y) = (x,-y)


windowTitle :: String
windowTitle = "Bouncing Ball"

windowSize :: (Int,Int)
windowSize = (640, 640)

windowLocation :: (Int,Int)
windowLocation = (10,10)

leftBoundary = -0.5 * fromIntegral (fst windowSize)
rightBoundary = 0.5 * fromIntegral (fst windowSize)
topBoundary = 0.5 * fromIntegral (snd windowSize)
bottomBoundary = -0.5 * fromIntegral (snd windowSize)

-- The entry point of the program.
main :: IO ()
main = play
    (InWindow windowTitle windowSize windowLocation)
    white
    60
    initialState
    render
    handleInput
    moveBall
