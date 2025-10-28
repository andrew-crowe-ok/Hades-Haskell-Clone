module Main where

import Brillo
import Brillo.Interface.Pure.Game hiding (KeyState)
import Types
import Constants

-- ---
-- 1. INITIAL STATES
-- ---


initialPlayer :: Player
initialPlayer = Player
  { playerPos = (0, 0)
  , playerVel = (0, 0)
  }

initialKeyState :: KeyState
initialKeyState = KeyState
  { keyW = False
  , keyA = False
  , keyS = False
  , keyD = False
  }

initialWorld :: World
initialWorld = World
  { player = initialPlayer
  , keys = initialKeyState
  }

-- ---
-- 2. GAME LOOP FUNCTIONS
-- ---

drawWorld :: World -> Picture
drawWorld world = Pictures
  [ drawRoom
  , drawPlayer (player world)
  ]


drawRoom :: Picture
drawRoom =
  Color white $ rectangleWire roomWidth roomHeight


drawPlayer :: Player -> Picture
drawPlayer p =
  Translate x y $ Color red $ circleSolid playerRadius
  where
    (x, y) = playerPos p


handleEvent :: Event -> World -> World
handleEvent event world =
  case event of

    -- Key Down events
    (EventKey (Char 'w') Down _ _) -> world { keys = (keys world) { keyW = True } }
    (EventKey (Char 'a') Down _ _) -> world { keys = (keys world) { keyA = True } }
    (EventKey (Char 's') Down _ _) -> world { keys = (keys world) { keyS = True } }
    (EventKey (Char 'd') Down _ _) -> world { keys = (keys world) { keyD = True } }

    -- Key Up events
    (EventKey (Char 'w') Up _ _) -> world { keys = (keys world) { keyW = False } }
    (EventKey (Char 'a') Up _ _) -> world { keys = (keys world) { keyA = False } }
    (EventKey (Char 's') Up _ _) -> world { keys = (keys world) { keyS = False } }
    (EventKey (Char 'd') Up _ _) -> world { keys = (keys world) { keyD = False } }

    -- Ignore all other events
    _ -> world


updateWorld :: Float -> World -> World
updateWorld dt world =
  world { player = newPlayer }
  where
    oldPlayer = player world
    ks = keys world
    (oldX, oldY) = playerPos oldPlayer

    vx = (if keyD ks then playerSpeed else 0) + (if keyA ks then -playerSpeed else 0)
    vy = (if keyW ks then playerSpeed else 0) + (if keyS ks then -playerSpeed else 0)
    newVel = (vx, vy)

    newX = oldX + vx * dt
    newY = oldY + vy * dt

    halfRoomW = roomWidth / 2 - playerRadius
    halfRoomH = roomHeight / 2 - playerRadius

    clampedX = max (-halfRoomW) (min halfRoomW newX)
    clampedY = max (-halfRoomH) (min halfRoomH newY)
    clampedPos = (clampedX, clampedY)

    newPlayer = oldPlayer { playerPos = clampedPos, playerVel = newVel }


-- ---
-- 3. MAIN
-- ---


main :: IO ()
main =
  play
    (InWindow "Hades Clone Demo" (windowWidth, windowHeight) (10, 10)) -- Window
    black                                 -- Background color
    60                                    -- Simulation steps per second
    initialWorld                          -- Initial world state
    drawWorld                             -- Function to draw the world
    handleEvent                           -- Function to handle events
    updateWorld