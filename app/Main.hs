module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding (KeyState)
import System.Random ( StdGen, getStdGen, split )
import Data.List (foldl', partition)
import Data.Maybe (isJust)

import Types
import Constants


main :: IO ()
main = do
  gen <- getStdGen
  play
    window
    bgColor
    fps
    (initialWorld gen)
    draw
    handleInput
    update


-- --- INITIAL STATE ---

-- A complete initial World.
initialWorld :: StdGen -> World
initialWorld gen = World
  { gameState    = MainMenu
  , player       = initialPlayer
  , currentRun   = initialRun
  , metaProgress = initialMeta
  , rng          = gen
  , keys         = initialKeyState
  , worldTime    = 0.0
  }


initialPlayer :: Player
initialPlayer = Player
  { playerPos     = (0, 0)
  , playerVel     = (0, 0)
  , currentHealth = 100
  , baseMaxHealth = 100
  , baseSpeed     = playerSpeed
  , baseDmgResist = 0.0
  , currentWeapon = Weapon Sword 10 2.0 0.0 -- Sword, 10 dmg, 2 atks/sec
  , currentBoons  = []
  , dashCount     = 3
  , dashCooldown  = 0
  , dashTimer     = 0
  , isDashing     = False
  , facingDir     = (1, 0)  -- Initially facing right
  }



initialKeyState :: KeyState
initialKeyState = KeyState
  { keyW        = False
  , keyA        = False
  , keyS        = False
  , keyD        = False
  , keyAttack   = False
  , keyDash     = False
  , keyInteract = False
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


-- A lookup table for base enemy stats.
-- Returns (baseHealth, baseSpeed, baseDamage, radius)
baseEnemyStats :: EnemyType -> (Int, Float, Int, Float)
baseEnemyStats MeleeBasic = (50, 100, 10, defaultEnemyRadius)
baseEnemyStats RangedTurrent = (30, 0, 8, defaultEnemyRadius)
-- Add new enemy types here, e.g.:
-- baseEnemyStats MeleeFast = (30, 150, 5, playerRadius)


-- A single test enemy, built from the stat database.
initialEnemy :: Enemy
initialEnemy =
  let eType = MeleeBasic
      (health, speed, dmg, radius) = baseEnemyStats eType
  in Enemy
    { enemyPos      = (200, 200)
    , eCurrentHealth = health
    , eBaseHealth    = health
    , eBaseSpeed     = speed
    , eBaseDmg       = dmg
    , enemyType     = eType
    , aiState       = Idle
    , enemyRadius   = radius
    }



initialMeta :: MetaProgress
initialMeta = MetaProgress
  { unlockedWeapons = [Sword]
  , permanentUpgrades = []
  , metaCurrency    = 0
  }


-- --- STATE MACHINE: Top-Level Dispatchers ---

-- Top-level drawing function (pure dispatcher)
draw :: World -> Picture
draw world =
  case gameState world of
    MainMenu    -> drawMenu world
    Running     -> drawGame world
    Paused      -> pictures [ drawGame world, drawPausedOverlay ]
    GameOver    -> drawGameOver world
    _           -> blank -- Handle other states like BoonSelection later

-- Top-level input handler (pure dispatcher)
handleInput :: Event -> World -> World
handleInput event world =
  case gameState world of
    MainMenu    -> handleMenuInput event world
    Running     -> handleRunningInput event world
    Paused      -> handlePausedInput event world
    GameOver    -> handleGameOverInput event world
    _           -> world

-- Top-level update function (pure dispatcher)
update :: Float -> World -> World
update dt world =
  let world' = world { worldTime = worldTime world + dt } -- Increment time
  in case gameState world' of
    Running -> updateGame dt world' -- Only update game if running
    _       -> world' -- All other states freeze game logic



-- --- STATE MACHINE: Handlers ---

-- Resets the player and run for a new game.
setupNewRun :: World -> World
setupNewRun world =
  let (gen1, gen2) = split (rng world) -- Use a new generator
  in world { player = initialPlayer
           , currentRun = initialRun -- TODO: This should use gen1 to build a chamber
           , rng = gen2
           }

-- Draws the main menu screen.
drawMenu :: World -> Picture
drawMenu _ =
  pictures
    [ translate (-200) 100 $ scale 0.3 0.3 $ color white $ text "Project Tartarus"
    , translate (-250) (-50) $ scale 0.2 0.2 $ color white $ text "Press [Enter] to Start"
    ]

-- Handles input for the main menu.
handleMenuInput :: Event -> World -> World
handleMenuInput (EventKey (SpecialKey KeyEnter) Down _ _) world =
  let world' = setupNewRun world -- Create the initial game state
  in world' { gameState = Running } -- Transition to 'Running'
handleMenuInput _ world = world

-- Draws the game over screen.
drawGameOver :: World -> Picture
drawGameOver _ =
  pictures
    [ translate (-150) 100 $ scale 0.3 0.3 $ color red $ text "YOU DIED"
    , translate (-300) (-50) $ scale 0.2 0.2 $ color white $ text "Press [Enter] to return to Menu"
    ]

-- Handles input for the game over screen.
handleGameOverInput :: Event -> World -> World
handleGameOverInput (EventKey (SpecialKey KeyEnter) Down _ _) world =
  world { gameState = MainMenu } -- Transition back to 'MainMenu'
handleGameOverInput _ world = world

-- Overlay for the 'Paused' state
drawPausedOverlay :: Picture
drawPausedOverlay = translate (-100) 0 $ scale 0.3 0.3 $ color white $ text "PAUSED"

-- Input for 'Paused' state
handlePausedInput :: Event -> World -> World
handlePausedInput (EventKey (SpecialKey KeyEnter) Down _ _) world = world { gameState = Running }
handlePausedInput _ world = world


-- --- DRAWING (Game) ---

-- Draw all elements of the running game.
drawGame :: World -> Picture
drawGame world = pictures
  [ drawRoom
  , drawPlayer (player world)
  , drawEnemies (enemies (currentChamber (currentRun world)))
  , drawProjectiles (projectiles (currentChamber (currentRun world)))
  , drawReward (reward (currentChamber (currentRun world)))
  , drawUI world
  ]


drawRoom :: Picture
drawRoom = color white $ rectangleWire roomWidth roomHeight


drawPlayer :: Player -> Picture
drawPlayer p =
  let (x, y) = playerPos p
      c = if isDashing p then cyan else red -- Flash cyan when dashing
  in translate x y $ color c $ circleSolid playerRadius


drawEnemies :: [Enemy] -> Picture
drawEnemies es = pictures $ map drawEnemy es


drawEnemy :: Enemy -> Picture
drawEnemy e =
  let (x, y) = enemyPos e
  in translate x y $ color chartreuse $ circleSolid (enemyRadius e)


drawProjectiles :: [Projectile] -> Picture
drawProjectiles ps = pictures $ map drawProjectile ps


drawProjectile :: Projectile -> Picture
drawProjectile p =
  let (x, y) = projPos p
      projColor = case projSource p of
                    FromPlayer -> cyan
                    FromEnemy  -> magenta
  in translate x y $ color projColor $ circleSolid (projRadius p)


-- ADDED: Draws the reward on the ground, if it exists.
drawReward :: Maybe Reward -> Picture
drawReward Nothing = blank
drawReward (Just (HealReward _ pos)) =
  let (x, y) = pos
  in translate x y $ color green $ circleSolid rewardRadius
drawReward (Just (SimpleBoon _ pos)) =
  let (x, y) = pos
  in translate x y $ color orange $ circleSolid rewardRadius
drawReward (Just (CurrencyReward _ pos)) =
  let (x, y) = pos
  in translate x y $ color yellow $ circleSolid rewardRadius
drawReward (Just (BoonChoice _ _ _)) =
  let (x, y) = (0, 100)
  in translate x y $ color (light orange) $ thickCircle rewardRadius 5


drawUI :: World -> Picture
drawUI world =
  let p = player world
      pStats = calculateStats p -- Get stats to show max health
      chamber = currentChamber (currentRun world)
      healthText = "Health: " ++ show (currentHealth p) ++ " / " ++ show (statMaxHealth pStats)
      dashText = "Dashes: " ++ show (dashCount p)
      clearedText = if isCleared chamber && isJust (reward chamber)
                    then translate (-150) 0 $ scale 0.2 0.2 $ color yellow $ text "Press [E] to collect"
                    else blank
  in pictures
    [ translate (-380) 280 $ scale 0.15 0.15 $ color white $ text healthText
    , translate (-380) 260 $ scale 0.15 0.15 $ color white $ text dashText
    , clearedText
    ]


-- --- INPUT HANDLING (Game) ---


-- Input handling for the 'Running' state.
-- This function *only* updates the KeyState.
handleRunningInput :: Event -> World -> World
handleRunningInput event world =
  let k = keys world
  in case event of
    -- Key presses
    EventKey (Char 'w') Down _ _ -> world { keys = k { keyW = True } }
    EventKey (Char 'a') Down _ _ -> world { keys = k { keyA = True } }
    EventKey (Char 's') Down _ _ -> world { keys = k { keyS = True } }
    EventKey (Char 'd') Down _ _ -> world { keys = k { keyD = True } }
    EventKey (Char 'f') Down _ _ -> world { keys = k { keyAttack = True } }
    EventKey (SpecialKey KeySpace) Down _ _ -> world { keys = k { keyDash = True } }
    EventKey (Char 'e') Down _ _ -> world { keys = k { keyInteract = True } }

    -- Key releases
    EventKey (Char 'w') Up   _ _ -> world { keys = k { keyW = False } }
    EventKey (Char 'a') Up   _ _ -> world { keys = k { keyA = False } }
    EventKey (Char 's') Up   _ _ -> world { keys = k { keyS = False } }
    EventKey (Char 'd') Up   _ _ -> world { keys = k { keyD = False } }
    EventKey (Char 'f') Up   _ _ -> world { keys = k { keyAttack = False } }
    EventKey (SpecialKey KeySpace) Up _ _ -> world { keys = k { keyDash = False } }
    EventKey (Char 'e') Up   _ _ -> world { keys = k { keyInteract = False } }

    -- Pause
    EventKey (SpecialKey KeyEnter) Down _ _ -> world { gameState = Paused }

    -- Default
    _ -> world


-- --- STAT CALCULATION (Boon System) ---

-- This is the core of the boon system.
-- It takes the player and computes their final stats for this frame.
calculateStats :: Player -> PlayerStats
calculateStats p =
  let w = currentWeapon p
      -- 1. Start with the base stats from player and weapon
      baseStats = PlayerStats
        { statMaxHealth    = baseMaxHealth p
        , statSpeed        = baseSpeed p
        , statDmgResist    = baseDmgResist p
        , statAttackDmg    = baseDmg w
        , statAttackRate   = baseAttackRate w
        , statDashCount    = maxDash
        }
      -- 2. Fold over the boon list, applying each one in order
  in foldl' applyBoon baseStats (currentBoons p)

-- Helper function to apply a single boon to the stats record.
applyBoon :: PlayerStats -> Boon -> PlayerStats
applyBoon stats (AttackDmg val) =
  stats { statAttackDmg = statAttackDmg stats + val }
applyBoon stats (AttackSpeed modifier) =
  stats { statAttackRate = statAttackRate stats * modifier }
applyBoon stats (ExtraHealth val) =
  stats { statMaxHealth = statMaxHealth stats + val }
applyBoon stats (MoveSpeed modifier) =
  stats { statSpeed = statSpeed stats * modifier }
applyBoon stats (DmgResist val) =
  -- Ensure resist cannot go above, say, 90%
  stats { statDmgResist = min 0.9 (statDmgResist stats + val) }
applyBoon stats (ExtraDash val) =
  stats { statDashCount = statDashCount stats + val }


-- --- UPDATE (Game) ---

-- Main game logic update pipeline.
updateGame :: Float -> World -> World
updateGame dt world =
  let p = player world

      -- 1. Calculate stats *once* at the beginning of the frame
      pStats = calculateStats p

      -- 2. Update player velocity based on key state
      world1 = updatePlayerVelocity world pStats

      -- 3. Check for and execute attacks (with cooldown)
      world2 = handleAttacks world1 pStats

      -- 4. Check for and execute dashes (with cooldown)
      world3 = handleDashing dt world2 pStats

      -- 5. Move player (applying dash velocity if dashing)
      world4 = movePlayer dt world3

      -- 6. Resolve physics collisions
      world5 = resolvePlayerEnemyCollisions world4

      -- 7. Update AI and move non-player entities
      world6 = updateAI dt world5
      world7 = moveEntities dt world6

      -- 8. Handle projectile hits and damage
      world8 = handleCollisions world7 pStats

      -- 9. Cleanup projectiles
      world9 = updateProjectiles dt world8

      -- 10. Check for player death
      finalWorld = checkPlayerDeath world9

  in finalWorld


-- Calculates player velocity based on currently held keys and stats.
updatePlayerVelocity :: World -> PlayerStats -> World
updatePlayerVelocity world pStats =
  let k = keys world
      p = player world
      speed = statSpeed pStats -- Use calculated stat

      -- Calculate X velocity
      vx = if keyA k && not (keyD k) then -speed
           else if keyD k && not (keyA k) then speed
           else 0 -- Neither or both are pressed

      -- Calculate Y velocity
      vy = if keyW k && not (keyS k) then speed
           else if keyS k && not (keyW k) then -speed
           else 0 -- Neither or both are pressed

      newVel = (vx, vy)
      newFacing = if vx /= 0 || vy /= 0 then normalize (vx, vy) else facingDir p

  in world { player = p { playerVel = newVel, facingDir = newFacing } }


-- Checks if the player is trying to attack and if the cooldown is ready.
handleAttacks :: World -> PlayerStats -> World
handleAttacks world pStats =
  let p = player world
      w = currentWeapon p
      k = keys world
      t = worldTime world

      -- Calculate time until next attack is ready
      cooldown = 1.0 / statAttackRate pStats
      nextAttackTime = lastAttack w + cooldown

  in if keyAttack k && t > nextAttackTime && not (isDashing p)
     -- Cooldown is ready, key is pressed, and not dashing: ATTACK
     then
       let world' = spawnProjectile world pStats -- Pass stats for damage
           -- Update the lastAttack time on the weapon
           newWeapon = w { lastAttack = t }
           newPlayer = p { currentWeapon = newWeapon }
       in world' { player = newPlayer }

     -- Either not attacking or on cooldown
     else world


-- Checks if the player is trying to dash and manages dash state.
handleDashing :: Float -> World -> PlayerStats -> World
handleDashing dt world pStats =
  let p = player world
      k = keys world
      maxDashes = statDashCount pStats

      -- 1. Check for dash start
      (isDashing', dashTimer', dashCount') =
        if keyDash k && dashCount p > 0 && not (isDashing p)
        then (True, dashDuration, dashCount p - 1) -- Start dash
        else (isDashing p, dashTimer p, dashCount p) -- Continue previous state

      -- 2. Update dash timer
      newDashTimer = max 0 (dashTimer' - dt)
      stillDashing = newDashTimer > 0

      -- 3. Regenerate dash if not full
      dashRegenTime = 1.0  -- seconds to regenerate one dash
      newDashCooldown = if dashCount' < maxDashes
                        then dashCooldown p + dt
                        else dashCooldown p

      (finalDashCount, finalCooldown) =
        if newDashCooldown >= dashRegenTime && dashCount' < maxDashes
        then (dashCount' + 1, newDashCooldown - dashRegenTime)
        else (dashCount', newDashCooldown)

      newPlayer = p { isDashing = stillDashing
                    , dashTimer = newDashTimer
                    , dashCooldown = finalCooldown
                    , dashCount = finalDashCount
                    }
  in world { player = newPlayer }


-- Creates a projectile originating from the player.
spawnProjectile :: World -> PlayerStats -> World
spawnProjectile world pStats =
  let p       = player world
      run     = currentRun world
      chamber = currentChamber run
      (px, py) = playerPos p
      (fx, fy) = facingDir p
      (dirX, dirY) = normalize (fx, fy)
      (vx, vy) = (dirX * projectileSpeed, dirY * projectileSpeed)

      newProj = Projectile
        { projPos    = (px + dirX * playerRadius, py + dirY * playerRadius)
        , projVel    = (vx, vy)
        , projDmg    = statAttackDmg pStats
        , projSource = FromPlayer
        , projRadius = projectileRadius
        , projTTL    = projectileTTL
        }

      newChamber = chamber { projectiles = newProj : projectiles chamber }
  in world { currentRun = run { currentChamber = newChamber } }


-- Update player position, applying smooth dash velocity
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


-- This function now takes the whole world and returns a new one
resolvePlayerEnemyCollisions :: World -> World
resolvePlayerEnemyCollisions world =
  let p = player world
      chamber = currentChamber (currentRun world)
      allEnemies = enemies chamber

      -- The accumulator is (updated Player, list of resolved enemies)
      (finalPlayer, resolvedEnemies) = foldl' resolveOne (p, []) allEnemies

      newChamber = chamber { enemies = resolvedEnemies }
  in world { player = finalPlayer, currentRun = (currentRun world) { currentChamber = newChamber } }


-- Helper for the fold, resolves collision between player and one enemy
resolveOne :: (Player, [Enemy]) -> Enemy -> (Player, [Enemy])
resolveOne (p, resolvedList) enemy =
  let (px, py) = playerPos p
      (ex, ey) = enemyPos enemy
      pRad     = playerRadius
      eRad     = enemyRadius enemy

      vecX = px - ex
      vecY = py - ey
      dist = magnitude (vecX, vecY) + 0.0001
      overlap = (pRad + eRad) - dist

  in if overlap > 0 && not (isDashing p) -- Make player invulnerable while dashing
     -- Overlap! Push both player and enemy
     then
       let dirX = vecX / dist
           dirY = vecY / dist
           pushAmount = overlap / 2

           newPx = px + dirX * pushAmount
           newPy = py + dirY * pushAmount
           newPlayer = p { playerPos = (newPx, newPy) }

           newEx = ex - dirX * pushAmount
           newEy = ey - dirY * pushAmount
           newEnemy = enemy { enemyPos = (newEx, newEy) }

       in (newPlayer, newEnemy : resolvedList)

     -- No overlap, or player is dashing
     else (p, enemy : resolvedList)


-- Update AI state for all enemies.
updateAI :: Float -> World -> World
updateAI dt world =
  let p = player world
      run = currentRun world
      chamber = currentChamber run
      newEnemies = map (updateEnemyAI p) (enemies chamber)
      newChamber = chamber { enemies = newEnemies }
  in world { currentRun = run { currentChamber = newChamber } }


-- Simple AI: If Idle, start Chasing the player.
updateEnemyAI :: Player -> Enemy -> Enemy
updateEnemyAI p e =
  case aiState e of
    Idle -> e { aiState = Chasing }
    _    -> e -- Keep chasing (or attacking, later)


-- Move all non-player entities.
moveEntities :: Float -> World -> World
moveEntities dt world =
  let p = player world
      run = currentRun world
      chamber = currentChamber run
      newEnemies = map (moveEnemy dt p) (enemies chamber)
      newProjs   = map (moveProjectile dt) (projectiles chamber)
      newChamber = chamber { enemies = newEnemies, projectiles = newProjs }
  in world { currentRun = run { currentChamber = newChamber } }


-- Move a single enemy.
moveEnemy :: Float -> Player -> Enemy -> Enemy
moveEnemy dt p e =
  case aiState e of
    Chasing ->
      let (px, py) = playerPos p
          (ex, ey) = enemyPos e
          vecToPlayer = (px - ex, py - ey)
          (dirX, dirY) = normalize vecToPlayer

          speed = eBaseSpeed e

          newX = ex + dirX * speed * dt
          newY = ey + dirY * speed * dt
      in e { enemyPos = (newX, newY) }
    _ -> e


-- Move a single projectile.
moveProjectile :: Float -> Projectile -> Projectile
moveProjectile dt p =
  let (px, py) = projPos p
      (vx, vy) = projVel p
      newX = px + vx * dt
      newY = py + vy * dt
      newTTL = projTTL p - dt
  in p { projPos = (newX, newY), projTTL = newTTL }


-- Filter out old projectiles.
updateProjectiles :: Float -> World -> World
updateProjectiles dt world =
  let run = currentRun world
      chamber = currentChamber run
      liveProjs = filter (\p -> projTTL p > 0) (projectiles chamber)
      newChamber = chamber { projectiles = liveProjs }
  in world { currentRun = run { currentChamber = newChamber } }


-- Check for projectile collisions and apply damage.
handleCollisions :: World -> PlayerStats -> World
handleCollisions world pStats =
  let p = player world
      run = currentRun world
      chamber = currentChamber run
      (playerProjs, enemyProjs) = partitionProjs (projectiles chamber)
      allEnemies = enemies chamber

      -- 1. Check player projectiles against enemies
      (survivingEnemies, hitProjs) = checkHits allEnemies playerProjs

      -- 2. Check enemy projectiles against player
      (playerAfterHits, projsThatHitPlayer) =
          checkPlayerHit p pStats enemyProjs

      -- 3. Check enemies against player (Melee Damage)
      -- (This is now handled by resolvePlayerEnemyCollisions,
      --  but we could add melee damage logic there too)

      -- Filter out projectiles that hit something
      survivingPlayerProjs = filter (`notElem` hitProjs) playerProjs
      survivingEnemyProjs  = filter (`notElem` projsThatHitPlayer) enemyProjs

      newChamber = chamber { enemies = survivingEnemies, projectiles = survivingPlayerProjs ++ survivingEnemyProjs }

  in world { player = playerAfterHits, currentRun = run { currentChamber = newChamber } }


-- Checks all enemy projectiles against the player
-- Returns (updated Player, projectiles that hit)
checkPlayerHit :: Player -> PlayerStats -> [Projectile] -> (Player, [Projectile])
checkPlayerHit p pStats enemyProjs =
  foldl' checkOneProj (p, []) enemyProjs
  where
    -- Helper for the fold
    checkOneProj :: (Player, [Projectile]) -> Projectile -> (Player, [Projectile])
    checkOneProj (player, hitProjs) proj =
      if isDashing player -- Invulnerable while dashing
      then (player, hitProjs)
      else if isColliding (playerPos player) playerRadius (projPos proj) (projRadius proj)
           then -- HIT! Apply damage.
             let -- Calculate damage after resistance
                 resist = statDmgResist pStats
                 dmgTaken = round $ fromIntegral (projDmg proj) * (1.0 - resist)
                 newHealth = currentHealth player - dmgTaken
                 newPlayer = player { currentHealth = newHealth }
             in (newPlayer, proj : hitProjs)
           else -- No hit
             (player, hitProjs)


-- Checks all enemies against all player projectiles.
-- Returns (surviving enemies, projectiles that hit)
checkHits :: [Enemy] -> [Projectile] -> ([Enemy], [Projectile])
checkHits allEnemies = foldr applyProjectileToEnemies (allEnemies, [])


-- Helper for checkHits. Applies one projectile to a list of enemies.
applyProjectileToEnemies :: Projectile -> ([Enemy], [Projectile]) -> ([Enemy], [Projectile])
applyProjectileToEnemies proj (enemies, hitProjs) =
  let (remainingEnemies, wasHit) = foldr (checkHit proj) ([], False) enemies
  in if wasHit
     then (remainingEnemies, proj : hitProjs) -- Add proj to hit list
     else (remainingEnemies, hitProjs)       -- Proj didn't hit, keep it


-- Helper for applyProjectileToEnemies. Checks one projectile against one enemy.
-- Returns (list of enemies to keep, bool if hit occurred)
checkHit :: Projectile -> Enemy -> ([Enemy], Bool) -> ([Enemy], Bool)
checkHit proj enemy (survivors, alreadyHit)
    | alreadyHit    = (enemy : survivors, True)
    | not collision = (enemy : survivors, False)
    | newHealth > 0 = (enemy { eCurrentHealth = newHealth } : survivors, True) -- Hit! Enemy survives
    | otherwise     = (survivors, True) -- Hit! Enemy dies
  where
    collision = isColliding (projPos proj) (projRadius proj) (enemyPos enemy) (enemyRadius enemy)
    newHealth = eCurrentHealth enemy - projDmg proj -- Use currentHealth


-- Checks for player death and transitions state.
checkPlayerDeath :: World -> World
checkPlayerDeath world =
  if currentHealth (player world) <= 0
  then world { gameState = GameOver }
  else world


-- Checks if the room is cleared and spawns a reward.
checkRoomCleared :: World -> World
checkRoomCleared world =
  let run = currentRun world
      chamber = currentChamber run
  in if not (isCleared chamber) && null (enemies chamber)
     then -- Room was JUST cleared!
          let (gen1, gen2) = split (rng world)
              (newReward, _gen) = generateReward gen1
              newChamber = chamber { isCleared = True, reward = Just newReward }
              newRun = run { currentChamber = newChamber }
          in world { currentRun = newRun, rng = gen2 }
     else world -- Room not cleared, or already was


-- Generates a reward. For now, always the AttackSpeed boon.
generateReward :: StdGen -> (Reward, StdGen)
generateReward gen =
  let boon = AttackSpeed 1.2 -- 20% attack speed increase
      pos = (0, 100) -- Spawn near the center-top
  in (SimpleBoon boon pos, gen) -- We pass 'gen' back, though it's unused here


-- Handles player interaction (e.g., collecting rewards).
handleInteraction :: World -> World
handleInteraction world =
  let k = keys world
      p = player world
      run = currentRun world
      chamber = currentChamber run
  in if keyInteract k && isCleared chamber
     then -- Player is pressing interact in a cleared room
          case reward chamber of
            Nothing -> world -- No reward to collect
            Just r  ->
              let (rPos, rRad) = getRewardPosRad r
              in if isColliding (playerPos p) playerRadius rPos rRad
                 then -- Player is colliding with reward! Collect it.
                      let world' = applyReward r world
                          newChamber = chamber { reward = Nothing } -- Remove reward
                          newRun = run { currentChamber = newChamber }
                      in world' { currentRun = newRun }
                 else world -- Pressing E but not on reward
     else world -- Not pressing E


-- Helper to get position and radius from any Reward type.
getRewardPosRad :: Reward -> ((Float, Float), Float)
getRewardPosRad (BoonChoice _ _ _) = ((0, 100), rewardRadius)
getRewardPosRad (SimpleBoon _ pos)   = (pos, rewardRadius)
getRewardPosRad (CurrencyReward _ pos) = (pos, rewardRadius)
getRewardPosRad (BoonChoice _ _ _) = ((0, 100), rewardRadius)


-- Applies the collected reward to the player.
applyReward :: Reward -> World -> World
applyReward (HealReward amt _) world =
  let p = player world
      pStats = calculateStats p
      newHealth = min (statMaxHealth pStats) (currentHealth p + amt)
  in world { player = p { currentHealth = newHealth } }


applyReward (SimpleBoon boon _) world =
  let p = player world
      newBoons = boon : currentBoons p
  in world { player = p { currentBoons = newBoons } }


applyReward (CurrencyReward amt _) world =
  let run = currentRun world
      newCurrency = runCurrency run + amt
  in world { currentRun = run { runCurrency = newCurrency } }

-- Handle BoonChoice by transitioning to a new state
applyReward (BoonChoice _ _ _) world =
  world { gameState = BoonSelection } -- We'll implement this state later


-- --- VECTOR MATH ---

-- Simple circle collision check.
isColliding :: (Float, Float) -> Float -> (Float, Float) -> Float -> Bool
isColliding (x1, y1) r1 (x2, y2) r2 =
  let dx = x1 - x2
      dy = y1 - y2
      distSq = dx*dx + dy*dy
      radiiSq = (r1 + r2) * (r1 + r2)
  in distSq < radiiSq


-- Partitions projectiles into player-sourced and enemy-sourced.
partitionProjs :: [Projectile] -> ([Projectile], [Projectile])
partitionProjs = partition (\p -> projSource p == FromPlayer)


-- Normalize a 2D vector.
normalize :: (Float, Float) -> (Float, Float)
normalize (x, y) =
  let len = sqrt (x*x + y*y)
  in if len == 0 then (0, 0) else (x / len, y / len)

-- Helper function to get the magnitude (length) of a vector.
magnitude :: (Float, Float) -> Float
magnitude (x, y) = sqrt (x*x + y*y)