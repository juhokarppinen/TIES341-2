{-
    Ball Bouncer

    By Juho Karppinen 2017

    A simple game of avoidance.


    Non-base dependencies:

    Graphics.Gloss
-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- Data type contains the ball's location and velocity and the state
-- of the keyboard.
data World = World
    {
        ball :: Ball,
        keys :: KeyboardState
    } deriving Show


-- The data type which contains the state of the arrow keys.
data KeyboardState = Keys 
    {
        up :: Bool,
        down :: Bool,
        left :: Bool,
        right :: Bool 
    } deriving Show


-- The data type which contains the state of the ball.
data Ball = Ball 
    {
        locB :: Point,
        velB :: Vector,
        radB :: Float
    } deriving Show


-- The data type which contains the state of a single obstacle.
data Obstacle = Obstacle
    {
        locO :: Point,
        velO :: Vector,
        radO :: Float
    } deriving Show


-- KeyboardStates corresponding to single arrow keys.
upKey :: KeyboardState
upKey = Keys True False False False

downKey :: KeyboardState
downKey = Keys False True False False

leftKey :: KeyboardState
leftKey = Keys False False True False

rightKey :: KeyboardState
rightKey = Keys False False False True

noKey :: KeyboardState
noKey = Keys False False False False


-- The amount of gravity imparted to the ball.
gravity :: Vector
gravity = (0, 0)


-- The amount of movement damping. 1 = no damping. 0 = Full damping.
damping :: Float
damping = 0.95


-- The amount of velocity addition imparted by keyboard movement.
effect :: Float
effect = 1


-- The size of the ball.
ballSize :: Float
ballSize = 25


-- The picture of the ball.
ballPic :: Picture
ballPic = circleSolid ballSize


-- The initial state of the world.
initialState :: World
initialState = World (Ball (0,0) (0,0) ballSize) noKey


-- Translate the ball according to its location.
render :: World -> Picture
render w = uncurry translate (locB . ball $ w) $ ballPic


-- Update the keyboard state according to keyboard input. Continuous keypresses
-- are recognized by handling down and up events separately.
handleInput :: Event -> World -> World
handleInput event world = 
    World 
        (Ball 
            (locB . ball $ world) 
            (velB . ball $ world) 
            ballSize) 
        keystate where
    keystate = case event of
        EventKey (SpecialKey KeyUp) Down _ _ ->
            orKeys (keys world) upKey
        EventKey (SpecialKey KeyDown) Down _ _ ->
            orKeys (keys world) downKey
        EventKey (SpecialKey KeyLeft) Down _ _ ->
            orKeys (keys world) leftKey
        EventKey (SpecialKey KeyRight) Down _ _ ->
            orKeys (keys world) rightKey
        EventKey (SpecialKey KeyUp) Up _ _ ->
            andKeys (keys world) $ negateKey upKey
        EventKey (SpecialKey KeyDown) Up _ _ ->
            andKeys (keys world) $ negateKey downKey
        EventKey (SpecialKey KeyLeft) Up _ _ ->
            andKeys (keys world) $ negateKey leftKey
        EventKey (SpecialKey KeyRight) Up _ _ ->
            andKeys (keys world) $ negateKey rightKey
        otherwise -> keys world


-- Perform an AND operation on two KeyboardStates.
andKeys :: KeyboardState -> KeyboardState -> KeyboardState
andKeys (Keys a1 a2 a3 a4) (Keys b1 b2 b3 b4) =
    Keys (a1 && b1) (a2 && b2) (a3 && b3) (a4 && b4)


-- Perform an OR operation on two KeyboardStates.
orKeys :: KeyboardState -> KeyboardState -> KeyboardState
orKeys (Keys a1 a2 a3 a4) (Keys b1 b2 b3 b4) =
    Keys (a1 || b1) (a2 || b2) (a3 || b3) (a4 || b4)


-- Perform a NOT operation on a KeyboardState.
negateKey :: KeyboardState -> KeyboardState
negateKey (Keys a b c d) = Keys (not a) (not b) (not c) (not d)


-- Handle ball movement.
moveBall :: Float -> World -> World
moveBall seconds world = 
    let
        -- Clamp the ball's x and y coordinates within the boundaries of the
        -- the window.
        location = locB . ball $ world
        x
            | fst location <= leftBoundary = leftBoundary
            | fst location >= rightBoundary = rightBoundary
            | otherwise = fst location  
        y 
            | snd location <= bottomBoundary = bottomBoundary
            | snd location >= topBoundary = topBoundary
            | otherwise = snd location

        -- Calculate the strength and direction of the new velocity.
        v = ((velB . ball $ world) .+ gravity .+ impulse) .* damping where
            impulse = (horizontal, vertical) where
                horizontal = case keys world of
                    Keys _ _ True False -> -effect
                    Keys _ _ False True -> effect
                    otherwise -> 0
                vertical = case keys world of
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

    in World (Ball newLoc newVel ballSize) (keys world)


-- Calculate the sum of two vectors.
(.+) :: Vector -> Vector -> Vector
(ax, ay) .+ (bx, by) = (ax + bx, ay + by)


-- Scale a vector by a float.
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


-- The boundaries of ball movement.
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
