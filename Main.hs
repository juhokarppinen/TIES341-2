{-
    Move-the-ball-avoid-the-wall

    By Juho Karppinen 2017

    A simple game of avoidance. Use arrow keys to move the ball around. Try
    to avoid the falling wall. Restart game with Space. Hit ESC to quit.


    Non-base dependencies:

    Graphics.Gloss
-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- The data type which contains the state of the ball, the obstacle and the
-- keyboard.
data World = World
    {
        ball :: Mover,
        obst :: Mover,
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


-- The data type which contains the state of a moving object.
data Mover = Mover 
    {
        loc :: Point,
        vel :: Vector
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


-- The amount of ball movement retention. 1 = full retention. 0 = no retention.
retention :: Float
retention = 0.95


-- The amount of velocity addition imparted by keyboard movement.
effect :: Float
effect = 1


-- The radius of the ball.
ballRadius :: Float
ballRadius = 25


-- The initial state of the world.
initialState :: World
initialState = World 
    (Mover (0,0) (0,0)) 
    (Mover (0,topBoundary + ballRadius) (0,-1))
    noKey


-- Render the world.
render :: World -> Picture
render world =
    let
        obstWidth = fromIntegral windowWidth
        obstHeight = 2 * ballRadius
        obstLeft = (loc $ obst world) .+ (-obstWidth / 2 - 2 * ballRadius, 0)
        obstRight = (loc $ obst world) .+ (obstWidth / 2 + 2 * ballRadius, 0)
        obstPict = rectangleSolid obstWidth obstHeight
        ballLoc = loc $ ball world
    in
        pictures
            [
                uncurry translate obstLeft obstPict,
                uncurry translate obstRight obstPict,
                uncurry translate ballLoc ballPic,
                restartText
            ] where
            ballPic
                | collision world = color red (circleSolid ballRadius)
                | otherwise = circleSolid ballRadius
            restartText
                | collision world = 
                    translate (-160) 0 $
                    scale 0.25 0.25 $ 
                    color green $ 
                    text "Press Space to restart"
                | otherwise = blank


-- Check whether the ball collides with an obstacle. Current implementation
-- doesn't allow pixel perfect collision detection. Collisions are
-- calculated using the rectangle surrounding the ball.
collision :: World -> Bool
collision world =
    let
        locationBall = loc $ ball world
        locationObst = loc $ obst world
        xB = fst locationBall
        yB = snd locationBall
        xO = fst locationObst
        yO = snd locationObst
        holeLeft = xO - 2 * ballRadius
        holeRight = xO + 2 * ballRadius
        obstTop = yO + ballRadius
        obstBottom = yO - ballRadius
        ballTop = yB + ballRadius
        ballBottom = yB - ballRadius
        ballLeft = xB - ballRadius
        ballRight = xB + ballRadius
        ballIsAtObstHeight =
            ballTop >= obstBottom && ballBottom <= obstTop
        ballIsAtHole = 
            ballLeft >= holeLeft && ballRight <= holeRight
    in
        ballIsAtObstHeight && not ballIsAtHole


-- Update the keyboard state according to keyboard input. Continuous keypresses
-- are recognized by handling down and up events separately.
handleInput :: Event -> World -> World
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world =
    if collision world then initialState else world
handleInput event world = 
    World 
        (Mover 
            (loc . ball $ world) 
            (vel . ball $ world))
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


-- Handle object movement. Collision stops the game.
updateWorld :: Float -> World -> World
updateWorld seconds world = if (collision world) then world else
    let
        -- Clamp the ball's x and y coordinates within the boundaries of the
        -- the window.
        locationB = loc $ ball world
        xB
            | fst locationB <= leftBoundary = leftBoundary
            | fst locationB >= rightBoundary = rightBoundary
            | otherwise = fst locationB  
        yB 
            | snd locationB <= bottomBoundary = bottomBoundary
            | snd locationB >= topBoundary = topBoundary
            | otherwise = snd locationB

        -- Calculate the strength and direction of the new velocity.
        vB = ((vel $ ball world) .+ impulse) .* retention where
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

        -- Set the obstacle's horizontal velocity such that the hole tends
        -- to move away from the ball but stays in the window. The obstacle
        -- constantly accelerates downwards, making the game more difficult
        -- the longer you play. 
        newVelO = (vel $ obst world) .+ (horizontal, -0.001) where
            xO = fst $ loc $ obst world
            xB = fst $ loc $ ball world
            horizontal 
                | xO < leftBoundary + 3 * ballRadius = 0.05
                | xO > rightBoundary - 3 * ballRadius = -0.05
                | xO < xB = -0.01 
                | otherwise = 0.01

        -- Set the obstacle's location based on its previous location and
        -- its velocity. If the obstacle moves past the window's bottom,
        -- move it above the window's top.
        newLocO 
            | snd (loc $ obst world) <= bottomBoundary - ballRadius * 2 =
                (0, topBoundary + ballRadius * 2)
            | otherwise = (loc $ obst world) .+ newVelO

    in World 
        (Mover newLocB newVelB) 
        (Mover newLocO newVelO)
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

windowWidth :: Int
windowWidth = 640

windowHeight :: Int
windowHeight = 640

windowSize :: (Int,Int)
windowSize = (windowWidth, windowHeight)

windowLocation :: (Int,Int)
windowLocation = (10,10)


-- The boundaries of ball movement.
leftBoundary = -0.5 * fromIntegral (fst windowSize) + ballRadius
rightBoundary = 0.5 * fromIntegral (fst windowSize) - ballRadius
topBoundary = 0.5 * fromIntegral (snd windowSize) - ballRadius
bottomBoundary = -0.5 * fromIntegral (snd windowSize) + ballRadius


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
