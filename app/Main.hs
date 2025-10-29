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


-- | Input handling for the 'Running' state.
handleRunningInput :: Event -> World -> World
handleRunningInput event world =
  let p = player world
      vel = playerVel p
  in case event of
    -- Movement keys update velocity
    EventKey (Char 'w') Down _ _ -> setPlayerVel (0, playerSpeed)
    EventKey (Char 'w') Up   _ _ -> setPlayerVel (0, -playerSpeed)
    EventKey (Char 's') Down _ _ -> setPlayerVel (0, -playerSpeed)
    EventKey (Char 's') Up   _ _ -> setPlayerVel (0, playerSpeed)
    EventKey (Char 'a') Down _ _ -> setPlayerVel (-playerSpeed, 0)
    EventKey (Char 'a') Up   _ _ -> setPlayerVel (playerSpeed, 0)
    EventKey (Char 'd') Down _ _ -> setPlayerVel (playerSpeed, 0)
    EventKey (Char 'd') Up   _ _ -> setPlayerVel (-playerSpeed, 0)
    
    -- Attack key
    EventKey (Char 'f') Down _ _ -> spawnProjectile world
    
    -- Default case
    _                            -> world

  where
    -- Helper to update player velocity in the nested world state
    setPlayerVel (vx, vy) =
      let p = player world
          (oldVx, oldVy) = playerVel p
          newVel = (oldVx + vx, oldVy + vy)
      in world { player = p { playerVel = newVel } }


-- | Creates a projectile originating from the player.
spawnProjectile :: World -> World
spawnProjectile world =
  let p = player world
      run = currentRun world
      chamber = currentChamber run
      (px, py) = playerPos p

      -- Simple projectile: always fires right
      (vx, vy) = (projectileSpeed, 0)

      newProj = Projectile
        { projPos    = (px + playerRadius, py) -- Spawn just outside player
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


-- | Main game logic update.
updateGame :: Float -> World -> World
updateGame dt world =
  let world1 = movePlayer dt world
      world2 = updateAI dt world1
      world3 = moveEntities dt world2
      world4 = handleCollisions world3
      world5 = updateProjectiles dt world4
  in world5


-- | Update player position based on velocity.
movePlayer :: Float -> World -> World
movePlayer dt world =
  let p = player world
      (px, py) = playerPos p
      (vx, vy) = playerVel p

      -- New position
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
