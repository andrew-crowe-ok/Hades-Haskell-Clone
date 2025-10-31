module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding (KeyState)
import System.Random ( StdGen, getStdGen, randomR )

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
    -- We'll add other states (Paused, GameOver) later
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
  in translate (-380) (280) $ scale 0.15 0.15 $ color white $ text healthText


-- --- INPUT HANDLING ---

-- | Top-level input handler.
handleInput :: Event -> World -> World
handleInput event world =
  case gameState world of
    Running -> handleRunningInput event world
    _       -> world -- No input in other states for now



handleRunningInput :: Event -> World -> World
handleRunningInput event world =
  case event of
    -- KEY DOWN
    EventKey (Char 'w') Down _ _ -> updateKeyState (\ks -> ks { keyW = True }) world
    EventKey (Char 'a') Down _ _ -> updateKeyState (\ks -> ks { keyA = True }) world
    EventKey (Char 's') Down _ _ -> updateKeyState (\ks -> ks { keyS = True }) world
    EventKey (Char 'd') Down _ _ -> updateKeyState (\ks -> ks { keyD = True }) world

    -- KEY UP
    EventKey (Char 'w') Up _ _ -> updateKeyState (\ks -> ks { keyW = False }) world
    EventKey (Char 'a') Up _ _ -> updateKeyState (\ks -> ks { keyA = False }) world
    EventKey (Char 's') Up _ _ -> updateKeyState (\ks -> ks { keyS = False }) world
    EventKey (Char 'd') Up _ _ -> updateKeyState (\ks -> ks { keyD = False }) world

    -- SHOOT
    EventKey (Char 'f') Down _ _ -> spawnProjectile world

    -- DASH
    EventKey (SpecialKey KeySpace) Down _ _ -> tryDash world

    _ -> world
  where
    -- Helper to update KeyState and then recalc movement
    updateKeyState f w =
      let newKeys = f (keys w)
      in updateMovementFromKeys w { keys = newKeys }

-- | Recomputes velocity and facing based on keys, unless dashing.
updateMovementFromKeys :: World -> World
updateMovementFromKeys world =
  let p = player world
  in if isDashing p
     then world  -- Ignore movement keys while dashing
     else
       let ks = keys world
           up    = if keyW ks then 1 else 0
           down  = if keyS ks then -1 else 0
           right = if keyD ks then 1 else 0
           left  = if keyA ks then -1 else 0

           -- Combined direction
           dx = fromIntegral (right + left)
           dy = fromIntegral (up + down)

           -- Normalize for diagonal movement
           (nx, ny) = normalize (dx, dy)

           newVel = (nx * playerSpeed, ny * playerSpeed)
           newFacing = if (dx, dy) == (0,0) then facingDir p else (nx, ny)
           newPlayer = p { playerVel = newVel, facingDir = newFacing }
       in world { player = newPlayer }


  --Dashing element
tryDash :: World -> World
tryDash world =
  let p = player world
  in if dashCount p > 0 && not (isDashing p)
        then world { player = p
            { isDashing    = True
            , dashTimer    = 0.2      -- dash lasts 0.2 s
            , dashCount    = dashCount p - 1
            , dashCooldown = if dashCount p == 1 then 1 else dashCooldown p
            }
        }
        else world





--  WORKS WITH WASD


-- | Creates a projectile originating from the player.
spawnProjectile :: World -> World
spawnProjectile world =
  let p       = player world
      run     = currentRun world
      chamber = currentChamber run
      (px, py) = playerPos p
      (fx, fy) = facingDir p   -- Get the player's facing direction

      -- Normalize facing direction so diagonal shots don't go faster
      len = sqrt (fx*fx + fy*fy)
      (nx, ny) = if len == 0 then (0,0) else (fx / len, fy / len)

      -- Compute projectile velocity
      (vx, vy) = (nx * projectileSpeed, ny * projectileSpeed)

      -- Create the projectile just outside the player
      newProj = Projectile
        { projPos    = (px + nx * playerRadius, py + ny * playerRadius)
        , projVel    = (vx, vy)
        , projDamage = damage (currentWeapon p)
        , projSource = FromPlayer
        , projRadius = projectileRadius
        , projTTL    = projectileTTL
        }

      -- Add the new projectile to the chamber
      newChamber = chamber { projectiles = newProj : projectiles chamber }
      newRun     = run { currentChamber = newChamber }
  in world { currentRun = newRun }



-- --- UPDATE FUNCTIONS ---

-- | Top-level update function.
update :: Float -> World -> World
update dt world =
  case gameState world of
    Running -> updateGame dt world
    _       -> world -- No updates in other states


-- | Main game logic update
updateGame :: Float -> World -> World
updateGame dt world =
  let world1 = world { player = updatePlayerTimers dt (player world) }
      world2 = movePlayer dt world1
      world3 = updateAI dt world2
      world4 = moveEntities dt world3
      world5 = handleCollisions world4
      world6 = updateProjectiles dt world5
  in world6


-- | Update player timers (dash, cooldown) every frame
updatePlayerTimers :: Float -> Player -> Player
updatePlayerTimers dt p =
  let p1 = if dashTimer p > 0
           then let t = dashTimer p - dt
                in p { dashTimer = max 0 t
                     , isDashing = t > 0
                     }
           else p
      p2 = if dashCooldown p1 > 0
           then let c = dashCooldown p1 - dt
                    newCount = if c <= 0 then 3 else dashCount p1
                in p1 { dashCooldown = max 0 c
                      , dashCount = newCount
                      }
           else p1
  in p2


-- | Update player position based on velocity or dash
movePlayer :: Float -> World -> World
movePlayer dt world =
  let p = player world
      (px, py) = playerPos p

      -- Use dash velocity if dashing, otherwise normal velocity
      (vx, vy) = if isDashing p
                 then let (fx, fy) = facingDir p
                      in (fx * dashSpeed, fy * dashSpeed)
                 else playerVel p

      -- Compute new position
      newX = px + vx * dt
      newY = py + vy * dt

      -- Clamp to room boundaries
      halfRoomW = roomWidth / 2
      halfRoomH = roomHeight / 2
      clampedX = max (-halfRoomW) (min halfRoomW newX)
      clampedY = max (-halfRoomH) (min halfRoomH newY)

      newPlayer = p { playerPos = (clampedX, clampedY) }
  in world { player = newPlayer }


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
checkHits allEnemies playerProjs =
  foldr (applyProjectileToEnemies) (allEnemies, []) playerProjs


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
checkHit proj enemy (survivors, alreadyHit) =
  if alreadyHit -- If this projectile already hit an enemy, just pass this enemy through
  then (enemy : survivors, True)
  else if isColliding (projPos proj) (projRadius proj) (enemyPos enemy) (enemyRadius enemy)
       then -- Hit! Apply damage.
         let newHealth = enemyHealth enemy - projDamage proj
         in if newHealth > 0
            then (enemy { enemyHealth = newHealth } : survivors, True) -- Enemy survives
            else (survivors, True) -- Enemy dies, don't add to list
       else (enemy : survivors, False) -- No hit


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


-- | Helper to 'partition' a list.
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)


-- --- VECTOR MATH ---

-- | Normalize a 2D vector.
normalize :: (Float, Float) -> (Float, Float)
normalize (x, y) =
  let len = sqrt (x*x + y*y)
  in if len == 0 then (0, 0) else (x / len, y / len)
