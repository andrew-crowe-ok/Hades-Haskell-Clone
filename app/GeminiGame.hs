module Main where

import Brillo
import Brillo.Interface.Pure.Game

-- | Data structure to hold the state of our world.
--   It contains the object's position and its velocity.
data World = World
  { objPos :: (Float, Float) -- (x, y) coordinates
  , objSiz :: (Float, Float)
  , objVel :: (Float, Float) -- (vx, vy) velocity in pixels/second
  , isObjectSelected :: Bool
  }

-- | The initial state of the world.
--   The object starts at the center (0,0) with a specific velocity.
initialWorld :: World
initialWorld = World
  { objPos = (0, 0)
  , objSiz = (20, 20)
  , objVel = (100, 70) -- Move 100px/sec right, 70px/sec up
  , isObjectSelected = False
  }

-- | The size of the window.
windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

-- | Half-dimensions, useful for boundary checking from the center (0,0).
halfWidth, halfHeight :: Float
halfWidth = fromIntegral windowWidth / 2
halfHeight = fromIntegral windowHeight / 2

-- | This function draws the world state as a Picture.
--   It translates the coordinate system to the object's position
--   and draws a solid red circle.
drawWorld :: World -> Picture
drawWorld world =
  let
    (x, y) = objPos world -- Get the object's position
    radius = 30          -- Define the object's radius
  in
    -- Translate moves the origin, then we draw the circle at (0,0)
    -- in the new translated coordinate system.
    Translate x y (Color red (circleSolid radius))

-- | This function handles user input events.
--   For this simple animation, we don't need to react to any events,
--   so we just return the world state unchanged.
handleEvent :: Event -> World -> World
handleEvent event world =
  case event of
    (EventKey (MouseButton LeftButton) Down _ mousePos) ->
      if isClickOnObject mousePos world
      then
        world { isObjectSelected = not (isObjectSelected world) }
      else
        world
    _ -> world

isClickOnObject :: (Float, Float) -> World -> Bool
isClickOnObject (clickX, clickY) world =
  let
    (objX, objY) = objPos world
    (objWidth, objHeight) = objSiz world

    -- Calculate the object's boundaries
    objLeft   = objX - (objWidth / 2)
    objRight  = objX + (objWidth / 2)
    objBottom = objY - (objHeight / 2)
    objTop    = objY + (objHeight / 2)

  in
    -- Check if the click is within the boundaries
    clickX >= objLeft   &&
    clickX <= objRight  &&
    clickY >= objBottom &&
    clickY <= objTop

-- | This function updates the world state for each simulation step.
--   'dt' (delta time) is the time in seconds since the last update.
updateWorld :: Float -> World -> World
updateWorld dt world =
  let
    (x, y)   = objPos world
    (sx, sy) = objSiz world
    (vx, vy) = objVel world
    radius   = 20 -- Must match the radius in drawWorld for accurate collision

    -- Calculate the new position based on velocity and delta time.
    x' = x + vx * dt
    y' = y + vy * dt

    (sx', sy') =
      if isClickOnObject
        then 

    -- Check for collision with vertical walls (left/right).
    (x'', vx') =
      if x' + radius > halfWidth || x' - radius < -halfWidth
      then (x, -vx) -- Bounce: reverse x-velocity, keep old x-position
      else (x', vx)  -- No bounce: update x-position, keep x-velocity

    -- Check for collision with horizontal walls (top/bottom).
    (y'', vy') =
      if y' + radius > halfHeight || y' - radius < -halfHeight
      then (y, -vy) -- Bounce: reverse y-velocity, keep old y-position
      else (y', vy)  -- No bounce: update y-position, keep y-velocity

    -- Return the new world state.
  in world
       { objPos = (x'', y'')
       , objSiz = (sx, sy)
       , objVel = (vx', vy')
       }

-- | Main function to run the simulation.
main :: IO ()
main =
  play
    (InWindow "Moving Object" (windowWidth, windowHeight) (10, 10)) -- Window
    white                                 -- Background color
    60                                    -- Simulation steps per second
    initialWorld                          -- Initial world state
    drawWorld                             -- Function to draw the world
    handleEvent                           -- Function to handle events
    updateWorld                           -- Function to update the world