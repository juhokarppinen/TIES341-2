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
        obst :: Obstacle,
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
        velO :: Vector
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


-- The amount of movement retention. 1 = full retention. 0 = no retention.
retention :: Float
retention = 0.95


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
initialState = World 
    (Ball (0,0) (0,0) ballSize) 
    (Obstacle (0,0) (0,-1))
    noKey


-- Render the world.
render :: World -> Picture
render world = pictures 
    [
        uncurry translate (locB $ ball world) $ ballPic,
        uncurry translate (locO $ obst world) $ rectangleSolid 100 (2 * ballSize)
    ]


-- Update the keyboard state according to keyboard input. Continuous keypresses
-- are recognized by handling down and up events separately.
handleInput :: Event -> World -> World
handleInput event world = 
    World 
        (Ball 
            (locB . ball $ world) 
            (velB . ball $ world) 
            ballSize) 
        (obst world)
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


-- Handle object movement.
updateWorld :: Float -> World -> World
updateWorld seconds world = 
    let
        -- Clamp the ball's x and y coordinates within the boundaries of the
        -- the window.
        locationB = locB $ ball world
        xB
            | fst locationB <= leftBoundary = leftBoundary
            | fst locationB >= rightBoundary = rightBoundary
            | otherwise = fst locationB  
        yB 
            | snd locationB <= bottomBoundary = bottomBoundary
            | snd locationB >= topBoundary = topBoundary
            | otherwise = snd locationB

        -- Calculate the strength and direction of the new velocity.
        vB = ((velB $ ball world) .+ gravity .+ impulse) .* retention where
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
        newVelB
            | xB <= leftBoundary || xB >= rightBoundary = negX vB
            | yB <= bottomBoundary || yB >= topBoundary = negY vB
            | otherwise = vB

        -- Set the ball's new location based on its previous location and
        -- its new velocity.    
        newLocB = (xB, yB) .+ newVelB


        vO = velO $ obst world

        -- Set the obstacles location based on its previous loation and
        -- its velocity.
        newLocO = (locO $ obst world) .+ vO

    in World 
        (Ball newLocB newVelB ballSize) 
        (Obstacle newLocO vO)
        (keys world)


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
leftBoundary = -0.5 * fromIntegral (fst windowSize) + ballSize
rightBoundary = 0.5 * fromIntegral (fst windowSize) - ballSize
topBoundary = 0.5 * fromIntegral (snd windowSize) - ballSize
bottomBoundary = -0.5 * fromIntegral (snd windowSize) + ballSize


-- The entry point of the program.
main :: IO ()
main = play
    (InWindow windowTitle windowSize windowLocation)
    white
    60
    initialState
    render
    handleInput
    updateWorld
