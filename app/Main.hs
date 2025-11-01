module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding (KeyState)
import System.Random ( StdGen, getStdGen )
import Data.List

import Types
import Constants

-- | Main entry point.
-- We now get a random generator from IO and pass it to initialWorld.
main :: IO ()
main = do
  gen <- getStdGen -- Get an initial random generator
  play
    window
    bgColor
    fps
    (initialWorld gen) -- Use it to build the world
    draw
    handleInput
    update


-- --- INITIAL STATE ---

-- | A complete initial World.
initialWorld :: StdGen -> World
initialWorld gen = World
  { gameState    = Running
  , player       = initialPlayer
  , currentRun   = initialRun
  , metaProgress = initialMeta
  , rng          = gen
  , keys         = initialKeyState
  }


initialPlayer :: Player
initialPlayer = Player
  { playerPos     = (0, 0)
  , playerVel     = (0, 0)
  , playerHealth  = 100
  , maxHealth     = 100
  , currentWeapon = Weapon Sword 10 2.0 0.0 -- Sword, 10 dmg, 2 atks/sec
  , currentBoons  = []
  , dashCount      = 2
  , dashCooldown   = 0
  , dashTimer      = 0
  , isDashing      = False
  , facingDir = (1, 0)  -- Initially facing right
  }



initialKeyState :: KeyState
initialKeyState = KeyState
  { keyW = False
  , keyA = False
  , keyS = False
  , keyD = False
  }


initialRun :: RunState
initialRun = RunState
  { currentChamber = Chamber
      { enemies     = [initialEnemy]
      , projectiles = []
      , reward      = Nothing
      , isCleared   = False
      }
  , chamberLevel   = 1
  , runCurrency    = 0
  }


  -- | A single test enemy
initialEnemy :: Enemy
initialEnemy = Enemy
  { enemyPos    = (200, 200)
  , enemyHealth = 50
  , enemyType   = MeleeBasic
  , aiState     = Idle
  , enemyRadius = defaultEnemyRadius
  }


initialMeta :: MetaProgress
initialMeta = MetaProgress
  { unlockedWeapons = [Sword]
  , permanentUpgrades = []
  , metaCurrency    = 0
  }


-- --- DRAWING ---

-- | Top-level drawing function.
draw :: World -> Picture
draw world =
  case gameState world of
    Running -> drawGame world
    Paused  -> pictures
        [ drawGame world
        , translate (-100) 0 $ scale 0.3 0.3 $ color white $ text "PAUSED"
        ]
    -- TODO: add a game over state
    _       -> drawGame world -- Default to drawing the game


-- | Draw all elements of the running game.
drawGame :: World -> Picture
drawGame world = pictures
  [ drawRoom
  , drawPlayer (player world)
  -- Use 'currentChamber' from 'currentRun'
  , drawEnemies (enemies (currentChamber (currentRun world)))
  , drawProjectiles (projectiles (currentChamber (currentRun world)))
  , drawUI world
  ]


-- | Draw the room boundaries
drawRoom :: Picture
drawRoom = color white $ rectangleWire roomWidth roomHeight


-- | Draw the player
drawPlayer :: Player -> Picture
drawPlayer p =
  let (x, y) = playerPos p
  in translate x y $ color red $ circleSolid playerRadius


-- | Draw all enemies
drawEnemies :: [Enemy] -> Picture
drawEnemies es = pictures $ map drawEnemy es


drawEnemy :: Enemy -> Picture
drawEnemy e =
  let (x, y) = enemyPos e
  in translate x y $ color chartreuse $ circleSolid (enemyRadius e)


-- | Draw all projectiles
drawProjectiles :: [Projectile] -> Picture
drawProjectiles ps = pictures $ map drawProjectile ps


drawProjectile :: Projectile -> Picture
drawProjectile p =
  let (x, y) = projPos p
      projColor = case projSource p of
                    FromPlayer -> cyan
                    FromEnemy  -> magenta
  in translate x y $ color projColor $ circleSolid (projRadius p)


-- | Draw the UI (e.g., player health)
drawUI :: World -> Picture
drawUI world =
  let p = player world
      healthText = "Health: " ++ show (playerHealth p)
  in translate (-380) 280 $ scale 0.15 0.15 $ color white $ text healthText


-- --- INPUT HANDLING ---

-- | Top-level input handler.
handleInput :: Event -> World -> World
handleInput event world =
  case gameState world of
    Running -> handleRunningInput event world
    Paused  -> handlePausedInput event world
    _       -> world -- No input in other states for now


handlePausedInput :: Event -> World -> World
handlePausedInput (EventKey (SpecialKey KeyEnter) Down _ _) world = world { gameState = Running }
handlePausedInput _ world = world

     


-- | Input handling for the 'Running' state.
handleRunningInput :: Event -> World -> World
handleRunningInput event world =
  case event of
    -- Key presses
    EventKey (Char 'w') Down _ _ -> updateKey (\ks -> ks { keyW = True }) world
    EventKey (Char 'a') Down _ _ -> updateKey (\ks -> ks { keyA = True }) world
    EventKey (Char 's') Down _ _ -> updateKey (\ks -> ks { keyS = True }) world
    EventKey (Char 'd') Down _ _ -> updateKey (\ks -> ks { keyD = True }) world

    -- Key releases
    EventKey (Char 'w') Up _ _ -> updateKey (\ks -> ks { keyW = False }) world
    EventKey (Char 'a') Up _ _ -> updateKey (\ks -> ks { keyA = False }) world
    EventKey (Char 's') Up _ _ -> updateKey (\ks -> ks { keyS = False }) world
    EventKey (Char 'd') Up _ _ -> updateKey (\ks -> ks { keyD = False }) world

    -- Attack key
    EventKey (Char 'f') Down _ _ -> spawnProjectile world

    -- Dash key (example: Left Shift)
    EventKey (SpecialKey KeySpace) Down _ _ -> tryDash world

    -- Pause
    EventKey (SpecialKey KeyEnter) Down _ _ -> world { gameState = Paused }

    -- Default
    _ -> world
  where
    -- Update keys and compute velocity/facing
    updateKey :: (KeyState -> KeyState) -> World -> World
    updateKey f world =
      let newKS = f (keys world)
          (vx, vy) = computeVel newKS
          newFacing = if vx /= 0 || vy /= 0 then normalize (vx, vy) else facingDir (player world)
          newPlayer = (player world) { playerVel = (vx, vy), facingDir = newFacing }
      in world { keys = newKS, player = newPlayer }

    -- Compute velocity from currently held keys
    computeVel :: KeyState -> (Float, Float)
    computeVel ks =
      let x = (if keyD ks then 1 else 0) - (if keyA ks then 1 else 0)
          y = (if keyW ks then 1 else 0) - (if keyS ks then 1 else 0)
      in (fromIntegral x * playerSpeed, fromIntegral y * playerSpeed)



-- Simple helper to multiply vector by scalar
mul :: (Float, Float) -> Float -> (Float, Float)
mul (x, y) s = (x*s, y*s)

dashPlayer :: Player -> Player
dashPlayer p =
  let (x, y) = playerPos p
      (fx, fy) = facingDir p
      dashDist = 50  -- fixed short dash distance
      maxDashCooldown = 0.5  -- half a second cooldown
  in p { playerPos = (x + fx * dashDist, y + fy * dashDist)
       , dashCooldown = maxDashCooldown
       }


-- | Creates a projectile originating from the player in the direction the player is facing.
spawnProjectile :: World -> World
spawnProjectile world =
  let p       = player world
      run     = currentRun world
      chamber = currentChamber run
      (px, py) = playerPos p
      (fx, fy) = facingDir p       -- Get the player's facing direction
      (dirX, dirY) = normalize (fx, fy)
      (vx, vy) = (dirX * projectileSpeed, dirY * projectileSpeed)

      -- Spawn just outside player in the facing direction
      newProj = Projectile
        { projPos    = (px + dirX * playerRadius, py + dirY * playerRadius)
        , projVel    = (vx, vy)
        , projDamage = damage (currentWeapon p)
        , projSource = FromPlayer
        , projRadius = projectileRadius
        , projTTL    = projectileTTL
        }

      newChamber = chamber { projectiles = newProj : projectiles chamber }
      newRun     = run { currentChamber = newChamber }
  in world { currentRun = newRun }



-- --- UPDATE FUNCTIONS ---

-- | Top-level update function.
update :: Float -> World -> World
update dt world =
  case gameState world of
    Running -> updateGame dt world
    Paused  -> world
    _       -> world -- No updates in other states


-- | Main game logic update.

updateGame :: Float -> World -> World
updateGame dt world =
  let world1 = movePlayer dt world
      world2 = resolvePlayerEnemyCollisions world1
      world3 = updateAI dt world2
      world4 = moveEntities dt world3
      world5 = handleCollisions world4
      world6 = updateProjectiles dt world5
      world7 = updateDash dt world6  -- 
  in world7

-- This function now takes the whole world and returns a new one
resolvePlayerEnemyCollisions :: World -> World
resolvePlayerEnemyCollisions world =
  let p = player world
      chamber = currentChamber (currentRun world)
      allEnemies = enemies chamber

      -- The accumulator is (updated Player, list of resolved enemies)
      -- We fold over all enemies, starting with the original player and an empty list
      (finalPlayer, resolvedEnemies) = foldl' resolveOne (p, []) allEnemies

      newChamber = chamber { enemies = resolvedEnemies }
  in world { player = finalPlayer, currentRun = (currentRun world) { currentChamber = newChamber } }


-- This is the new helper function for the fold
-- It takes the accumulator (Player, [Enemy]) and one Enemy
-- It returns the new accumulator
resolveOne :: (Player, [Enemy]) -> Enemy -> (Player, [Enemy])
resolveOne (p, resolvedList) enemy =
  let (px, py) = playerPos p
      (ex, ey) = enemyPos enemy
      pRad     = playerRadius
      eRad     = enemyRadius enemy -- Use the specific enemy's radius

      vecX = px - ex
      vecY = py - ey
      dist = magnitude (vecX, vecY) + 0.0001 -- Add epsilon to avoid div by zero
      overlap = (pRad + eRad) - dist

  in if overlap > 0
     -- Overlap! Push both player and enemy by half the overlap
     then
       let dirX = vecX / dist -- Normalized push direction
           dirY = vecY / dist
           pushAmount = overlap / 2

           -- Push the player
           newPx = px + dirX * pushAmount
           newPy = py + dirY * pushAmount
           newPlayer = p { playerPos = (newPx, newPy) }

           -- Push the enemy (in the opposite direction)
           newEx = ex - dirX * pushAmount
           newEy = ey - dirY * pushAmount
           newEnemy = enemy { enemyPos = (newEx, newEy) }

       in (newPlayer, newEnemy : resolvedList) -- Return new player and add new enemy to list

     -- No overlap, return the player unchanged and add the original enemy to the list
     else (p, enemy : resolvedList)


-- This function is defined inside `resolvePlayerEnemyCollisions` (in its `where` clause)
-- or at the top level, passing in playerRadius.
checkAndPush :: (Float, Float) -> Enemy -> (Float, Float)
checkAndPush currentPPos enemy =
  let (px, py) = currentPPos
      (ex, ey) = enemyPos enemy
      eRad     = enemyRadius enemy -- Make sure to use the enemy's radius
      pRad     = playerRadius      -- This constant must be available

      -- 1. Calculate vector from enemy to player
      vecX = px - ex
      vecY = py - ey

      -- 2. Calculate distance
      -- You'll need a magnitude helper: magnitude (x, y) = sqrt (x*x + y*y)
      -- Add a small value to avoid division by zero if dist is 0
      dist = magnitude (vecX, vecY) + 0.0001

      -- 3. Calculate overlap
      overlap = (pRad + eRad) - dist

  in if overlap > 0
     -- 4. They are overlapping! Push the player.
     then
       -- 4a. Find the push direction (normalize the vector)
       let dirX = vecX / dist
           dirY = vecY / dist

       -- 4b. Calculate new position by pushing by 'overlap' amount
           newX = px + dirX * overlap
           newY = py + dirY * overlap
       in (newX, newY)

     -- 5. No overlap, return position unchanged for the next check
     else currentPPos


magnitude :: (Float, Float) -> Float
magnitude (x, y) = sqrt (x*x + y*y)


-- | Called when player presses the dash key (Space)
tryDash :: World -> World
tryDash world =
  let p = player world
  in if dashCount p > 0 && not (isDashing p)
     then
       let newPlayer = p { isDashing = True
                         , dashTimer = dashDuration
                         , dashCount = dashCount p - 1
                         }
       in world { player = newPlayer }
     else world

-- | Dash duration in seconds
dashDuration :: Float
dashDuration = 0.1

-- | Multiplier for dash speed (how much faster than normal)
dashMultiplier :: Float
dashMultiplier = 3.5

-- | Update player position, applying smooth dash velocity
movePlayer :: Float -> World -> World
movePlayer dt world =
  let p = player world
      (px, py) = playerPos p
      (vx, vy) = playerVel p
      -- If dashing, apply dash speed multiplier
      (dx, dy) = if isDashing p
                 then (vx * dashMultiplier * dt, vy * dashMultiplier * dt)
                 else (vx * dt, vy * dt)
      newX = px + dx
      newY = py + dy
      -- Keep player inside the room
      halfRoomW = roomWidth / 2
      halfRoomH = roomHeight / 2
      clampedX = max (-halfRoomW) (min halfRoomW newX)
      clampedY = max (-halfRoomH) (min halfRoomH newY)
      newPlayer = p { playerPos = (clampedX, clampedY) }
  in world { player = newPlayer }

-- | Update dash timers and regenerate dash counts
updateDash :: Float -> World -> World
updateDash dt world =
  let p = player world

      -- Reduce dash timer
      newDashTimer = max 0 (dashTimer p - dt)
      stillDashing = newDashTimer > 0

      -- Regenerate dash if not full
      dashRegenTime = 1.0  -- seconds to regenerate one dash
      newDashCooldown = if dashCount p < maxDash then dashCooldown p + dt else dashCooldown p
      (finalDashCount, finalCooldown) =
        if newDashCooldown >= dashRegenTime && dashCount p < maxDash
        then (dashCount p + 1, newDashCooldown - dashRegenTime)
        else (dashCount p, newDashCooldown)

      newPlayer = p { dashTimer = newDashTimer
                    , isDashing = stillDashing
                    , dashCooldown = finalCooldown
                    , dashCount = finalDashCount
                    }
  in world { player = newPlayer }

-- | Maximum dashes player can hold
maxDash :: Int
maxDash = 3

-- | Update AI state for all enemies.
updateAI :: Float -> World -> World
updateAI dt world =
  let p = player world
      run = currentRun world
      chamber = currentChamber run

      -- Update AI state for each enemy
      newEnemies = map (updateEnemyAI p) (enemies chamber)

      newChamber = chamber { enemies = newEnemies }
      newRun     = run { currentChamber = newChamber }
  in world { currentRun = newRun }


-- | Simple AI: If Idle, start Chasing the player.
updateEnemyAI :: Player -> Enemy -> Enemy
updateEnemyAI p e =
  case aiState e of
    Idle -> e { aiState = Chasing }
    _    -> e -- Keep chasing (or attacking, later)


-- | Move all non-player entities.
moveEntities :: Float -> World -> World
moveEntities dt world =
  let p = player world
      run = currentRun world
      chamber = currentChamber run

      -- Move enemies and projectiles
      newEnemies = map (moveEnemy dt p) (enemies chamber)
      newProjs   = map (moveProjectile dt) (projectiles chamber)

      newChamber = chamber { enemies = newEnemies, projectiles = newProjs }
      newRun     = run { currentChamber = newChamber }
  in world { currentRun = newRun }


-- | Move a single enemy.
moveEnemy :: Float -> Player -> Enemy -> Enemy
moveEnemy dt p e =
  case aiState e of
    Chasing ->
      let (px, py) = playerPos p
          (ex, ey) = enemyPos e

          -- Vector from enemy to player
          vecToPlayer = (px - ex, py - ey)
          -- Normalized direction vector
          (dirX, dirY) = normalize vecToPlayer

          -- Enemy speed (hardcoded for now)
          speed = 100

          newX = ex + dirX * speed * dt
          newY = ey + dirY * speed * dt
      in e { enemyPos = (newX, newY) }

    _ -> e -- Don't move if Idle or Attacking


-- | Move a single projectile.
moveProjectile :: Float -> Projectile -> Projectile
moveProjectile dt p =
  let (px, py) = projPos p
      (vx, vy) = projVel p
      newX = px + vx * dt
      newY = py + vy * dt
      newTTL = projTTL p - dt
  in p { projPos = (newX, newY), projTTL = newTTL }


-- | Filter out old projectiles.
updateProjectiles :: Float -> World -> World
updateProjectiles dt world =
  let run = currentRun world
      chamber = currentChamber run
      allProjs = projectiles chamber

      -- Keep projectiles that still have Time To Live
      liveProjs = filter (\p -> projTTL p > 0) allProjs

      newChamber = chamber { projectiles = liveProjs }
      newRun     = run { currentChamber = newChamber }
  in world { currentRun = newRun }


-- | Check for collisions and apply damage.
handleCollisions :: World -> World
handleCollisions world =
  let p = player world
      run = currentRun world
      chamber = currentChamber run

      -- Separate player and enemy projectiles
      (playerProjs, enemyProjs) = partitionProjs (projectiles chamber)
      allEnemies = enemies chamber

      -- 1. Check player projectiles against enemies
      -- For each enemy, find projectiles that hit it
      (survivingEnemies, hitProjs) = checkHits allEnemies playerProjs

      -- 2. Check enemy projectiles against player (TODO)
      -- For now, just apply damage to player

      -- 3. Check enemies against player (TODO)

      -- Filter out projectiles that hit something
      survivingProjs = filter (`notElem` hitProjs) playerProjs ++ enemyProjs

      -- Update world state
      newChamber = chamber { enemies = survivingEnemies, projectiles = survivingProjs }
      newRun     = run { currentChamber = newChamber }
      -- We'd also update player health here if they were hit

  in world { currentRun = newRun }


-- | Checks all enemies against all player projectiles.
-- Returns (surviving enemies, projectiles that hit)
checkHits :: [Enemy] -> [Projectile] -> ([Enemy], [Projectile])
checkHits allEnemies = foldr applyProjectileToEnemies (allEnemies, [])


-- | Helper for checkHits. Applies one projectile to a list of enemies.
applyProjectileToEnemies :: Projectile -> ([Enemy], [Projectile]) -> ([Enemy], [Projectile])
applyProjectileToEnemies proj (enemies, hitProjs) =
  let (remainingEnemies, wasHit) = foldr (checkHit proj) ([], False) enemies
  in if wasHit
     then (remainingEnemies, proj : hitProjs) -- Add proj to hit list
     else (remainingEnemies, hitProjs)       -- Proj didn't hit, keep it


-- | Helper for applyProjectileToEnemies. Checks one projectile against one enemy.
-- Returns (list of enemies to keep, bool if hit occurred)
checkHit :: Projectile -> Enemy -> ([Enemy], Bool) -> ([Enemy], Bool)
checkHit proj enemy (survivors, alreadyHit)
    | alreadyHit    = (enemy : survivors, True) -- Projectile already hit; pass enemy through
    | not collision = (enemy : survivors, False) -- No collision
    | newHealth > 0 = (enemy { enemyHealth = newHealth } : survivors, True) -- Hit! Enemy survives
    | otherwise     = (survivors, True) -- Hit! Enemy dies
  where
    collision = isColliding (projPos proj) (projRadius proj) (enemyPos enemy) (enemyRadius enemy)
    newHealth = enemyHealth enemy - projDamage proj


-- | Simple circle collision check.
isColliding :: (Float, Float) -> Float -> (Float, Float) -> Float -> Bool
isColliding (x1, y1) r1 (x2, y2) r2 =
  let dx = x1 - x2
      dy = y1 - y2
      distSq = dx*dx + dy*dy
      radiiSq = (r1 + r2) * (r1 + r2)
  in distSq < radiiSq


-- | Partitions projectiles into player-sourced and enemy-sourced.
partitionProjs :: [Projectile] -> ([Projectile], [Projectile])
partitionProjs = partition (\p -> projSource p == FromPlayer)


-- --- VECTOR MATH ---

-- | Normalize a 2D vector.
normalize :: (Float, Float) -> (Float, Float)
normalize (x, y) =
  let len = sqrt (x*x + y*y)
  in if len == 0 then (0, 0) else (x / len, y / len)

