module Main where

import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- Data type contains the ball's location and velocity and the state
-- of the keyboard.
data World = World
    {
        loc :: Point,
        vel :: Vector,
        key :: KeyboardState
    } deriving Show


data KeyboardState = Keys 
    {
        upKey :: Bool,
        downKey :: Bool,
        leftKey :: Bool,
        rightKey :: Bool 
    } deriving Show


-- The amount of gravity imparted to the ball.
gravity :: Vector
gravity = (0, 0)


-- The amount of movement damping. 1 = no damping. 0 = Full damping.
damping :: Float
damping = 0.95


-- The amount of velocity addition imparted by keyboard movement.
effect :: Float
effect = 1


ballSize :: Float
ballSize = 25


ballPic :: Picture
ballPic = circleSolid ballSize


-- The initial state of the world.
initialState :: World
initialState = World (0,0) (0,0) (Keys False False False False)


-- Translate the ball according to its location.
render :: World -> Picture
render w = uncurry translate (loc w) $ ballPic


-- Update the keyboard state according to keyboard input.
handleInput :: Event -> World -> World
handleInput event world = World (loc world) (vel world) keys where
    keys = case event of
        EventKey (SpecialKey KeyUp) Down _ _ ->
            orKeys (key world) (Keys True False False False)
        EventKey (SpecialKey KeyDown) Down _ _ ->
            orKeys (key world) (Keys False True False False)
        EventKey (SpecialKey KeyLeft) Down _ _ ->
            orKeys (key world) (Keys False False True False)
        EventKey (SpecialKey KeyRight) Down _ _ ->
            orKeys (key world) (Keys False False False True)
        EventKey (SpecialKey KeyUp) Up _ _ ->
            andKeys (key world) (Keys False True True True)
        EventKey (SpecialKey KeyDown) Up _ _ ->
            andKeys (key world) (Keys True False True True)
        EventKey (SpecialKey KeyLeft) Up _ _ ->
            andKeys (key world) (Keys True True False True)
        EventKey (SpecialKey KeyRight) Up _ _ ->
            andKeys (key world) (Keys True True True False)
        otherwise -> key world


-- Perform the AND operation for two KeyboardStates.
andKeys :: KeyboardState -> KeyboardState -> KeyboardState
andKeys (Keys a1 a2 a3 a4) (Keys b1 b2 b3 b4) =
    Keys (a1 && b1) (a2 && b2) (a3 && b3) (a4 && b4)


-- Perform the OR operation for two KeyboardStates.
orKeys :: KeyboardState -> KeyboardState -> KeyboardState
orKeys (Keys a1 a2 a3 a4) (Keys b1 b2 b3 b4) =
    Keys (a1 || b1) (a2 || b2) (a3 || b3) (a4 || b4)


moveBall :: Float -> World -> World
moveBall seconds world = 
    let
        -- Clamp the ball's x and y coordinates within the boundaries of the
        -- the window.
        x
            | fst (loc world) <= leftBoundary = leftBoundary
            | fst (loc world) >= rightBoundary = rightBoundary
            | otherwise = fst (loc world)  
        y 
            | snd (loc world) <= bottomBoundary = bottomBoundary
            | snd (loc world) >= topBoundary = topBoundary
            | otherwise = snd (loc world)

        -- Calculate the strength and direction of the new velocity.
        v = ((vel world) .+ gravity .+ impulse) .* damping where
            impulse = (horizontal, vertical) where
                horizontal = case key world of
                    Keys _ _ True False -> -effect
                    Keys _ _ False True -> effect
                    otherwise -> 0
                vertical = case key world of
                    Keys True False _ _ -> effect
                    Keys False True _ _ -> -effect
                    otherwise -> 0

        -- Make the ball bounce off walls by negating a velocity component.
        newVel
            | x <= leftBoundary || x >= rightBoundary = negX v
            | y <= bottomBoundary || y >= topBoundary = negY v
            | otherwise = v

        -- Set the ball's new location based on its previous location and
        -- its new velocity.    
        newLoc = (x, y) .+ newVel

    in World newLoc newVel (key world)


-- Sum of two vectors.
(.+) :: Vector -> Vector -> Vector
(ax, ay) .+ (bx, by) = (ax + bx, ay + by)


-- Scale a vector.
(.*) :: Vector -> Float -> Vector
(x, y) .* s = (x * s, y * s)


-- Negate the x component of a vector.
negX :: Vector -> Vector
negX (x,y) = (-x,y)


-- Negate the y component of a vector.
negY :: Vector -> Vector
negY (x,y) = (x,-y)


windowTitle :: String
windowTitle = "Bouncing Ball"

windowSize :: (Int,Int)
windowSize = (640, 640)

windowLocation :: (Int,Int)
windowLocation = (10,10)

leftBoundary = -0.5 * fromIntegral (fst windowSize) + 0.75 * ballSize
rightBoundary = 0.5 * fromIntegral (fst windowSize) - 0.75 * ballSize
topBoundary = 0.5 * fromIntegral (snd windowSize) - 0.75 * ballSize
bottomBoundary = -0.5 * fromIntegral (snd windowSize) + 0.75 * ballSize

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
