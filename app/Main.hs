module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding (KeyState)
import System.Random ( StdGen, getStdGen, split, randomR )
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
  , enemySpawnTimer = 3.0   -- first enemy appears after 3 seconds
  , keys         = initialKeyState
  , mousePos = (0, 0)  -- start mouse at (0,0)
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
  , currentWeapon = Weapon Sword 10 2.0 0.0 0.0-- Sword, 10 dmg, 2 atks/sec
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
  , keyMelee    = False
  , keyDash     = False
  , keyInteract = False
  }


initialRun :: RunState
initialRun = RunState
  { currentChamber = Chamber
      { enemies     = [initialEnemy MeleeBasic (100, 200)]
      , projectiles = []
      , rewards     = []
      , isCleared   = False
      }
  , chamberLevel   = 1
  , runCurrency    = 0
  }



-- A lookup table for base enemy stats.
-- Returns (baseHealth, baseSpeed, baseDamage, radius)
baseEnemyStats :: EnemyType -> (Int, Float, Int, Float)
baseEnemyStats MeleeBasic = (10, 100, 10, defaultEnemyRadius)
baseEnemyStats RangedTurret = (30, 0, 8, defaultEnemyRadius)
-- Add new enemy types here, e.g.:
-- baseEnemyStats MeleeFast = (30, 150, 5, playerRadius)


-- A single test enemy, built from the stat database.
initialEnemy :: EnemyType -> (Float, Float) -> Enemy
initialEnemy et pos = case et of
  MeleeBasic -> Enemy
    { enemyPos       = pos
    , eCurrentHealth = 10
    , eBaseHealth    = 10
    , eBaseSpeed     = 80
    , eBaseDmg       = 2
    , enemyType      = MeleeBasic
    , aiState        = Idle
    , enemyRadius    = 12
    , hitTimer       = 0
    , enemyFacingDir      = (0,1)
    }
  RangedTurret -> Enemy
    { enemyPos       = pos
    , eCurrentHealth = 8
    , eBaseHealth    = 8
    , eBaseSpeed     = 50        -- slower than MeleeBasic
    , eBaseDmg       = 3          -- damage per arrow
    , enemyType      = RangedTurret
    , aiState        = Idle
    , enemyRadius    = 12
    , hitTimer       = 4          -- starts ready to shoot every 4s
    , enemyFacingDir      = (0,1)
    }
  ShieldCharger -> Enemy
    { enemyPos       = pos
    , eCurrentHealth = 20
    , eBaseHealth    = 20
    , eBaseSpeed     = 40         -- slow, but can charge
    , eBaseDmg       = 4
    , enemyType      = ShieldCharger
    , aiState        = Idle
    , enemyRadius    = 16
    , hitTimer       = 0
    , enemyFacingDir      = (0,1)      -- initial facing up
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
  , drawPlayer (player world) world
  , drawEnemies (enemies (currentChamber (currentRun world)))
  , drawProjectiles (projectiles (currentChamber (currentRun world)))
  , drawRewards (rewards (currentChamber (currentRun world))) 
  , drawUI world
  ]

drawRoom :: Picture
drawRoom = color white $ rectangleWire roomWidth roomHeight

drawPlayer :: Player -> World -> Picture
drawPlayer p world =
  let (x, y) = playerPos p
      c       = if isDashing p then cyan else red
      pStats  = calculateStats p  -- ⚠ calculate PlayerStats here
  in pictures
       [ translate x y $ color c $ circleSolid playerRadius
       , drawSword pStats p world  -- ⚠ pass PlayerStats, Player, World
       ]


-- Draw the player's sword as a rectangle pointing toward the mouse

drawSword :: PlayerStats -> Player -> World -> Picture
drawSword pStats p world
  | not (keyMelee (keys world)) || isDashing p = Blank
  | otherwise =
      let (px, py) = playerPos p
          (mx, my) = mousePos world
          (dx, dy) = normalize (subV (mx, my) (px, py))

          -- Base sword size
          baseLength, swordWidth :: Float
          baseLength = 40
          swordWidth  = 8

          -- Add length for all LongSword boons
          swordLength = baseLength + statSwordLength pStats

          -- Sword center is half of length along direction
          centerOffset = scaleV (swordLength / 2) (dx, dy)
          swordCenter = addV (px, py) centerOffset

          -- Rotation in degrees
          angle = atan2 dy dx * 180 / pi

          -- Sword rectangle in white
          swordRect = color white $ rectangleSolid swordLength swordWidth
      in translate (fst swordCenter) (snd swordCenter) $ rotate angle swordRect



drawEnemies :: [Enemy] -> Picture
drawEnemies = Pictures . map drawEnemy

drawProjectiles :: [Projectile] -> Picture
drawProjectiles ps = Pictures $ map drawProjectile ps

drawProjectile :: Projectile -> Picture
drawProjectile p =
  let (x, y) = projPos p
      projColor = case projSource p of
                    FromPlayer -> cyan
                    FromEnemy  -> magenta
  in translate x y $ color projColor $ circleSolid (projRadius p)



drawEnemy :: Enemy -> Picture
drawEnemy e =
  case enemyType e of

    -- ---------------------- MeleeBasic ----------------------
    MeleeBasic ->
      let radius = enemyRadius e
          circlePic = Color green $ circleSolid radius
      in translateEx e circlePic

    -- ---------------------- RangedTurret ----------------------
    RangedTurret ->
      let triangle = Color yellow $ polygon [(-10,-10),(10,-10),(0,10)]
      in translateEx e triangle

    -- ---------------------- ShieldCharger ----------------------
    ShieldCharger ->
      let size = enemyRadius e * 2
          baseSquare = Color blue $ polygon [(-size,-size),(size,-size),(size,size),(-size,size)]
      -- Red front indicator
          frontHeight = size / 2
          frontWidth  = size * 1.5
          frontIndicator = Color red $ polygon [(-frontWidth/2,0),(frontWidth/2,0),(frontWidth/2,frontHeight),(-frontWidth/2,frontHeight)]
      
      -- Offset front indicator in front of enemy
          shieldOffsetDist = enemyRadius e + frontHeight / 2
          (dx, dy) = scaleV shieldOffsetDist (enemyFacingDir e)
          positionedFront = translate (fst (enemyPos e) + dx) (snd (enemyPos e) + dy) frontIndicator
      in translateEx e baseSquare <> positionedFront


boonColor :: Boon -> Color
boonColor boon = case boon of
  AttackDmg _        -> red
  AttackSpeed _      -> orange        -- new feature: bright orange
  MoveSpeed _        -> blue          -- new feature: bright blue
  ExtraHealth _      -> green
  DmgResist _        -> violet
  ExtraDash _        -> cyan
  LongSword _        -> rose
  MultiShot _        -> azure
  RapidFire _        -> magenta
  SniperShot _ _     -> chartreuse
  RotatingShield _ _ -> greyN 0.5


-- ADDED: Draws the reward on the ground, if it exists.
-- Draw all rewards in the chamber
drawRewards :: [Reward] -> Picture
drawRewards = pictures . map drawSingleReward

-- Draw a single reward with different colors per Boon type
drawSingleReward :: Reward -> Picture
drawSingleReward r =
  let ((x, y), rad) = getRewardPosRad r
      col = case r of
              HealReward _ _       -> green
              CurrencyReward _ _   -> yellow
              SimpleBoon b _       -> boonColor b
              BoonChoice _ _ _     -> light orange
  in translate x y $ color col $ circleSolid rad

drawUI :: World -> Picture
drawUI world =
  let p = player world
      pStats = calculateStats p -- Get stats to show max health
      chamber = currentChamber (currentRun world)
      healthText = "Health: " ++ show (currentHealth p) ++ " / " ++ show (statMaxHealth pStats)
      dashText = "Dashes: " ++ show (dashCount p)
      clearedText = if isCleared chamber && not (null (rewards chamber))
                    then translate (-150) 0 $ scale 0.2 0.2 $ color yellow $ text "Press [E] to collect"
                    else blank
  in pictures
    [ translate (-380) 280 $ scale 0.15 0.15 $ color white $ text healthText
    , translate (-380) 260 $ scale 0.15 0.15 $ color white $ text dashText
    , clearedText
    ]

translateEx :: Enemy -> Picture -> Picture
translateEx e pic = translate x y pic
  where
    (x,y) = enemyPos e
-- --- INPUT HANDLING (Game) ---


-- Input handling for the 'Running' state.
-- This function *only* updates the KeyState.
-- Input handling for the 'Running' state
handleRunningInput :: Event -> World -> World
handleRunningInput event world =
  let k = keys world
  in case event of
    -- Key presses
    EventKey (Char 'w') Down _ _ -> world { keys = k { keyW = True } }
    EventKey (Char 'a') Down _ _ -> world { keys = k { keyA = True } }
    EventKey (Char 's') Down _ _ -> world { keys = k { keyS = True } }
    EventKey (Char 'd') Down _ _ -> world { keys = k { keyD = True } }
    EventKey (SpecialKey KeySpace) Down _ _ -> world { keys = k { keyDash = True } }
    EventKey (Char 'e') Down _ _ -> world { keys = k { keyInteract = True } }

    -- Key releases
    EventKey (Char 'w') Up _ _ -> world { keys = k { keyW = False } }
    EventKey (Char 'a') Up _ _ -> world { keys = k { keyA = False } }
    EventKey (Char 's') Up _ _ -> world { keys = k { keyS = False } }
    EventKey (Char 'd') Up _ _ -> world { keys = k { keyD = False } }
    EventKey (SpecialKey KeySpace) Up _ _ -> world { keys = k { keyDash = False } }
    EventKey (Char 'e') Up _ _ -> world { keys = k { keyInteract = False } }

    -- Pause
    EventKey (SpecialKey KeyEnter) Down _ _ -> world { gameState = Paused }

    -- Mouse presses
    EventKey (MouseButton LeftButton) Down _ _ -> world { keys = k { keyAttack = True } }
    EventKey (MouseButton LeftButton) Up _ _ -> world { keys = k { keyAttack = False } }
    EventKey (MouseButton RightButton) Down _ _ -> world { keys = k { keyMelee = True } }
    EventKey (MouseButton RightButton) Up _ _ -> world { keys = k { keyMelee = False } }

    -- Mouse movement
    EventMotion (mx, my) -> world { mousePos = (mx, my) }

    -- Default
    _ -> world

updatePlayerFacing :: World -> World
updatePlayerFacing world =
  let p = player world
      (px, py) = playerPos p
      (mx, my) = mousePos world
      dir = normalize (mx - px, my - py)
      newPlayer = p { facingDir = dir }
  in world { player = newPlayer }
    

-- --- STAT CALCULATION (Boon System) ---

-- This is the core of the boon system.
-- It takes the player and computes their final stats for this frame.
calculateStats :: Player -> PlayerStats
calculateStats p =
    let w = currentWeapon p
        baseStats = PlayerStats
            { statMaxHealth  = baseMaxHealth p
            , statSpeed      = baseSpeed p
            , statDmgResist  = baseDmgResist p
            , statAttackDmg  = baseDmg w
            , statAttackRate = baseAttackRate w
            , statSwordLength = 40        -- base sword length
            , statDashCount  = maxDash
            }

        applyBoon stats boon = case boon of
            AttackDmg n    -> stats { statAttackDmg   = statAttackDmg stats + n }
            LongSword n    -> stats { statAttackDmg   = statAttackDmg stats + n
                                    , statSwordLength = statSwordLength stats + fromIntegral n }
            AttackSpeed f  -> stats { statAttackRate  = statAttackRate stats * (1 + f) }
            ExtraHealth n  -> stats { statMaxHealth   = statMaxHealth stats + n }
            MoveSpeed f    -> stats { statSpeed        = statSpeed stats * (1 + f) }
            DmgResist f    -> stats { statDmgResist   = min 0.9 (statDmgResist stats + f) }
            ExtraDash n    -> stats { statDashCount   = statDashCount stats + n }
            _              -> stats  -- other boons ignored here

    in foldl' applyBoon baseStats (currentBoons p)


applyBoon :: PlayerStats -> Boon -> PlayerStats
applyBoon stats (AttackDmg val) = stats { statAttackDmg = statAttackDmg stats + val }
applyBoon stats (LongSword val) = stats { statAttackDmg = statAttackDmg stats + val
                                        , statSwordLength = statSwordLength stats + fromIntegral val }
applyBoon stats (AttackSpeed f) = stats { statAttackRate = statAttackRate stats * (1 + f) }
applyBoon stats (ExtraHealth val) = stats { statMaxHealth = statMaxHealth stats + val }
applyBoon stats (MoveSpeed f) = stats { statSpeed = statSpeed stats * (1 + f) }
applyBoon stats (DmgResist f) = stats { statDmgResist = min 0.9 (statDmgResist stats + f) }
applyBoon stats (ExtraDash val) = stats { statDashCount = statDashCount stats + val }
applyBoon stats _ = stats



-- --- UPDATE (Game) ---

updateGame :: Float -> World -> World
updateGame dt world =
  let
      -- 0. Update player facing to follow mouse
      world0 = updatePlayerFacing world

      -- 1. Calculate stats
      pStats = calculateStats (player world0)

      -- 2. Update player velocity based on key state
      world1 = updatePlayerVelocity world0 pStats

      -- 3. Handle dashing
      world2 = handleDashing dt world1 pStats

      -- 4. Move player (apply dash & velocity)
      world3 = movePlayer dt world2

      -- 5. Handle shooting (projectiles)
      world4 = handleAttacks world3 pStats

      -- 6. Handle melee (sword)
      world5 = handleMelee world4 pStats

      -- 7. Handle interactions
      world6 = handleInteraction world5

      -- 8. Resolve collisions
      world7 = resolvePlayerEnemyCollisions pStats world6
      world7b = spawnMoreEnemies dt world7

      -- 9. Update AI
      world8 = updateAI dt world7b
      world9 = moveEntities dt world8

      -- 10. Handle projectile collisions
      world10 = handleCollisions world9 pStats

      -- 11. Check room cleared
      world11 = checkRoomCleared world10

      -- 12. Update projectiles
      world12 = updateProjectiles dt world11

      -- 13. Check player death
      finalWorld = checkPlayerDeath world12

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


-- Handle ranged attack (projectile) using mouse left click
handleAttacks :: World -> PlayerStats -> World
handleAttacks world pStats =
  let p = player world
      w = currentWeapon p
      k = keys world
      t = worldTime world
      cooldown = 1.0 / statAttackRate pStats
      ready = t - lastAttack w >= cooldown
  in if not (keyAttack k) || not ready || isDashing p
     then world
     else
       let (px, py) = playerPos p
           (mx, my) = mousePos world
           (dirX, dirY) = normalize (subV (mx, my) (px, py))
           (vx, vy) = (dirX * projectileSpeed, dirY * projectileSpeed)

           newProj = Projectile
             { projPos    = (px + dirX * playerRadius, py + dirY * playerRadius)
             , projVel    = (vx, vy)
             , projDmg    = statAttackDmg pStats
             , projSource = FromPlayer
             , projRadius = projectileRadius
             , projTTL    = projectileTTL
             }

           run = currentRun world
           chamber = currentChamber run
           newChamber = chamber { projectiles = newProj : projectiles chamber }

           newW = w { lastAttack = t }
           newP = p { currentWeapon = newW }
       in world { player = newP, currentRun = run { currentChamber = newChamber } }


-- Handle melee (sword) attack using mouse right click

handleMelee :: World -> PlayerStats -> World
handleMelee world pStats =
  let p = player world
      w = currentWeapon p
      k = keys world
      t = worldTime world
      cooldown = 1.0 / statAttackRate pStats
      ready = t - lastAttack w >= cooldown
  in if not (keyMelee k) || not ready || weaponType w /= Sword || isDashing p
     then world
     else
       let (px, py) = playerPos p
           (mx, my) = mousePos world
           (fx, fy) = normalize (subV (mx, my) (px, py))
           swordRange = statSwordLength pStats
           swordRadius = 36
           hitCenter = addV (px, py) (scaleV (0.5 * swordRange) (fx, fy)) -- stabbing motion

           run = currentRun world
           chamber = currentChamber run
           allEnemies = enemies chamber
           dmg = statAttackDmg pStats
           gen = rng world

           (updatedEnemies, newRewards, gen') =
             foldl
               (\(accEnemies, accRewards, g) e ->
                  let dist = magnitude (subV (enemyPos e) hitCenter)
                  in if dist <= swordRadius
                     then
                       let newHP = eCurrentHealth e - dmg
                       in if newHP <= 0
                          then
                            let allBoons = [ AttackDmg 1, AttackSpeed 1.2, ExtraHealth 10, MoveSpeed 0.5
                                           , DmgResist 0.1, ExtraDash 1, LongSword 1, MultiShot 1
                                           , RapidFire 0.5, SniperShot 2 1.5, RotatingShield 30 5 ]
                                (boonIndex, g') = randomR (0, length allBoons - 1) g
                                newBoon = SimpleBoon (allBoons !! boonIndex) (enemyPos e)
                            in (accEnemies, newBoon : accRewards, g')
                          else (accEnemies ++ [e { eCurrentHealth = newHP }], accRewards, g)
                     else (accEnemies ++ [e], accRewards, g)
               ) ([], [], gen) allEnemies

           newW = w { lastAttack = t }
           newP = p { currentWeapon = newW }
           newChamber = chamber { enemies = updatedEnemies, rewards = rewards chamber ++ newRewards }
       in world { player = newP, currentRun = run { currentChamber = newChamber }, rng = gen' }





swordDuration :: Float
swordDuration = 0.15  -- seconds for stabbing animation

currentSwordLength :: Player -> PlayerStats -> Float
currentSwordLength p stats =
    let t = swordTimer p
        maxL = statSwordLength stats
    in min maxL (maxL * (t / swordDuration))  -- linear growth



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
      (mx, my) = mousePos world        -- <- use mouse position directly
      (dirX, dirY) = normalize (mx - px, my - py)
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


spawnEnemyProjectile :: Enemy -> World -> World
spawnEnemyProjectile e world =
  let (ex, ey) = enemyPos e
      pPos     = playerPos (player world)
      (dirX, dirY) = normalize (subV pPos (ex, ey))
      newProj = Projectile
        { projPos    = (ex + dirX * enemyRadius e, ey + dirY * enemyRadius e)
        , projVel    = (dirX * 200, dirY * 200)  -- projectile speed
        , projDmg    = eBaseDmg e
        , projSource = FromEnemy
        , projRadius = 5
        , projTTL    = 5
        }
      chamber = currentChamber (currentRun world)
      run     = currentRun world
      newChamber = chamber { projectiles = newProj : projectiles chamber }
  in world { currentRun = run { currentChamber = newChamber } }


-- This function now takes PlayerStats and the whole world and returns a new one
resolvePlayerEnemyCollisions :: PlayerStats -> World -> World
resolvePlayerEnemyCollisions pStats world =
  let p = player world
      chamber = currentChamber (currentRun world)
      allEnemies = enemies chamber

      -- Accumulator: (updated Player, list of resolved enemies)
      -- foldl' + append in resolveOne preserves order
      (finalPlayer, resolvedEnemies) = foldl' (resolveOne pStats) (p, []) allEnemies

      newChamber = chamber { enemies = resolvedEnemies }
      newRun     = (currentRun world) { currentChamber = newChamber }
  in world { player = finalPlayer, currentRun = newRun }


-- Resolve collisions between all enemies in the chamber
resolveEnemyCollisions :: [Enemy] -> [Enemy]
resolveEnemyCollisions enemiesList = foldl resolveOne [] enemiesList
  where
    resolveOne :: [Enemy] -> Enemy -> [Enemy]
    resolveOne resolved e =
      let e' = foldl (pushApart) e resolved
      in resolved ++ [e']

    pushApart :: Enemy -> Enemy -> Enemy
    pushApart e1 e2 =
      let (x1, y1) = enemyPos e1
          (x2, y2) = enemyPos e2
          r1 = enemyRadius e1
          r2 = enemyRadius e2
          dx = x1 - x2
          dy = y1 - y2
          dist = sqrt (dx*dx + dy*dy) + 0.0001
          overlap = (r1 + r2) - dist
      in if overlap > 0
         then
           let pushX = (dx / dist) * (overlap / 2)
               pushY = (dy / dist) * (overlap / 2)
           in e1 { enemyPos = (x1 + pushX, y1 + pushY) }
         else e1
resolveOne pStats (p, acc) e =
  let (px, py) = playerPos p
      (ex, ey) = enemyPos e
      collision = isColliding (px, py) 20 (ex, ey) (enemyRadius e)
      dmg = eBaseDmg e
      isFacingPlayer = not (isHitFromBehind e (px, py))
  in
  if not collision then
      (p, acc ++ [e])

  -- 🛡 ShieldCharger in Charging mode → deal damage always
  else if enemyType e == ShieldCharger && aiState e == Charging then
      let newHealth = currentHealth p - dmg
          p' = p { currentHealth = newHealth }
      in (p', acc ++ [e])

  -- 🛡 ShieldCharger shield front blocks melee damage TO PLAYER?
  else if enemyType e == ShieldCharger && isFacingPlayer then
      -- shield is blocking, so player takes normal collision damage
      let newHealth = currentHealth p - dmg
          p' = p { currentHealth = newHealth }
      in (p', acc ++ [e])

  -- default: enemy deals touch damage
  else
      let newHealth = currentHealth p - dmg
          p' = p { currentHealth = newHealth }
      in (p', acc ++ [e])


spawnEnemy :: World -> World
spawnEnemy world =
  let run      = currentRun world
      chamber  = currentChamber run
      gen      = rng world

      -- Random position within room bounds
      (x, gen1) = randomR (-roomWidth/2 + 30, roomWidth/2 - 30) gen
      (y, gen2) = randomR (-roomHeight/2 + 30, roomHeight/2 - 30) gen1

      existingEnemies = enemies chamber
      enemyCount      = length existingEnemies

      -- Randomly pick enemy type
      (etypeIndex, gen3) = randomR (0 :: Int, 2) gen2
      enemyTypeChosen = case etypeIndex of
        0 -> MeleeBasic
        1 -> RangedTurret
        2 -> ShieldCharger
        _ -> MeleeBasic  -- fallback

      -- Base enemy (all start from initialEnemy template)
      baseEnemy = initialEnemy enemyTypeChosen (100, 200)

      -- Adjust stats based on type
      newEnemy = case enemyTypeChosen of
        MeleeBasic ->
          baseEnemy
            { enemyPos       = (x, y)
            , eCurrentHealth = eBaseHealth baseEnemy + enemyCount * 5
            , eBaseHealth    = eBaseHealth baseEnemy + enemyCount * 5
            , eBaseDmg       = eBaseDmg baseEnemy + enemyCount `div` 2
            , eBaseSpeed     = eBaseSpeed baseEnemy
            }

        RangedTurret ->
          baseEnemy
            { enemyPos       = (x, y)
            , eCurrentHealth = 30 + enemyCount * 3
            , eBaseHealth    = 30 + enemyCount * 3
            , eBaseDmg       = 5 + enemyCount `div` 3
            , eBaseSpeed     = eBaseSpeed baseEnemy * 0.7 -- slower
            }

        ShieldCharger ->
          baseEnemy
            { enemyPos       = (x, y)
            , eCurrentHealth = 50 + enemyCount * 5
            , eBaseHealth    = 50 + enemyCount * 5
            , eBaseDmg       = 8 + enemyCount `div` 2
            , eBaseSpeed     = eBaseSpeed baseEnemy * 0.5 -- slow
            }

      updatedChamber = chamber { enemies = newEnemy : existingEnemies }
      updatedRun     = run { currentChamber = updatedChamber }

  in world
       { currentRun      = updatedRun
       , rng             = gen3
       , enemySpawnTimer = 3.0
       }

spawnMoreEnemies :: Float -> World -> World
spawnMoreEnemies dt world =
  let timer = enemySpawnTimer world - dt
  in if timer > 0
       then world { enemySpawnTimer = timer }
       else spawnEnemy world

-- Update AI state for all enemies and handle attacks
updateAI :: Float -> World -> World
updateAI dt world =
  let p = player world
      run = currentRun world
      chamber = currentChamber run

      -- Process each enemy
      (newEnemies, newWorld) = foldl (processEnemy dt p) ([], world) (enemies chamber)

      newChamber = chamber { enemies = newEnemies }
  in newWorld { currentRun = run { currentChamber = newChamber } }

-- Process a single enemy for AI, movement, and attacks
processEnemy :: Float -> Player -> ([Enemy], World) -> Enemy -> ([Enemy], World)
processEnemy dt p (accEnemies, world) e =
  let (updatedEnemy, world') = updateSingleEnemyAI dt p e world
  in (accEnemies ++ [updatedEnemy], world')

-- Update a single enemy
updateSingleEnemyAI :: Float -> Player -> Enemy -> World -> (Enemy, World)
updateSingleEnemyAI dt p e world =
  case enemyType e of

    -- ---------------------- Melee Basic ----------------------
    MeleeBasic ->
      let e'  = if aiState e == Idle then e { aiState = Chasing } else e
          e'' = moveMeleeBasic dt p e'
      in (e'', world)

    -- ---------------------- Ranged Turret ----------------------
    RangedTurret ->
      let eAIUpdated = if aiState e == Idle then e { aiState = Chasing } else e
          eMoved = moveRangedTurret dt p eAIUpdated
          dirToPlayer = normalize $ subV (playerPos p) (enemyPos eMoved)
          eFacing = eMoved { enemyFacingDir = dirToPlayer, hitTimer = hitTimer eMoved + dt }
      in if hitTimer eFacing >= 4.0
         then
           let (eFired, world') = fireAtPlayer world p eFacing
           in (eFired { hitTimer = 0 }, world')  -- fire and reset
         else
           (eFacing, world)

    -- ---------------------- Shield Charger ----------------------
    ShieldCharger ->
      let dirToPlayer = normalize $ subV (playerPos p) (enemyPos e)
          eAIUpdated = case aiState e of
                         Idle    -> e { aiState = Chasing, enemyFacingDir = dirToPlayer }
                         Chasing -> e { enemyFacingDir = dirToPlayer }
                         _       -> e
          eMoved = moveShieldCharger dt p eAIUpdated
      in (eMoved, world)
-- ---------------------- Movement Helpers ----------------------

-- Melee Basic moves normally
moveMeleeBasic :: Float -> Player -> Enemy -> Enemy
moveMeleeBasic dt p e =
  if aiState e == Chasing
  then
    let (px, py) = playerPos p
        (ex, ey) = enemyPos e
        (dirX, dirY) = normalize (px - ex, py - ey)
        newPos = (ex + dirX * eBaseSpeed e * dt, ey + dirY * eBaseSpeed e * dt)
    in e { enemyPos = newPos }
  else e

-- Ranged Turret moves slowly toward player
moveRangedTurret :: Float -> Player -> Enemy -> Enemy
moveRangedTurret dt p e =
  let (px, py) = playerPos p
      (ex, ey) = enemyPos e
      (dirX, dirY) = normalize (px - ex, py - ey)
      speedFactor = 1.5
      newPos = (ex + dirX * eBaseSpeed e * speedFactor * dt,
                ey + dirY * eBaseSpeed e * speedFactor * dt)
  in e { enemyPos = newPos }

-- Shield Charger movement (charges when close)
moveShieldCharger :: Float -> Player -> Enemy -> Enemy
moveShieldCharger dt p e =
  let (px, py) = playerPos p
      (ex, ey) = enemyPos e
      baseSpeed = eBaseSpeed e
  in case aiState e of

       -- ---------------------- CHASING ----------------------
       Chasing ->
         let vecToPlayer = normalize (subV (px, py) (ex, ey))
         in if magnitude (subV (px, py) (ex, ey)) < 150
            then e { aiState = Charging
                   , chargeTimer = 0
                   , enemyFacingDir = vecToPlayer  -- store direction at start of charge
                   }
            else e { enemyPos = addV (ex, ey) (scaleV (baseSpeed * dt) vecToPlayer)
                   , enemyFacingDir = vecToPlayer
                   }

       -- ---------------------- CHARGING ----------------------
       Charging ->
         let dirToCharge = enemyFacingDir e       -- <-- use locked direction
             moveSpeed = baseSpeed * 9
             newPos = addV (ex, ey) (scaleV (moveSpeed * dt) dirToCharge)
             newTimer = chargeTimer e + dt
         in if newTimer >= 1
            then e { enemyPos = newPos
                   , aiState = Recovering
                   , chargeTimer = 0
                   }
            else e { enemyPos = newPos
                   , chargeTimer = newTimer
                   , enemyFacingDir = dirToCharge   -- keep locked
                   }

       -- ---------------------- RECOVERING ----------------------
       Recovering ->
         let dirToPlayer = normalize (subV (px, py) (ex, ey))
             moveSpeed = baseSpeed * 0.5
             newPos = addV (ex, ey) (scaleV (moveSpeed * dt) dirToPlayer)
             newTimer = chargeTimer e + dt
         in if newTimer >= 2
            then e { aiState = Chasing
                   , chargeTimer = 0
                   }
            else e { enemyPos = newPos
                   , chargeTimer = newTimer
                   , enemyFacingDir = dirToPlayer
                   }

       Idle -> e

-- Update AI state for a single enemy
updateEnemyAI :: Player -> Enemy -> Enemy
updateEnemyAI p e =
  case enemyType e of

    MeleeBasic ->
      case aiState e of
        Idle -> e { aiState = Chasing }
        _    -> e

    RangedTurret ->
      let e' = if aiState e == Idle then e { aiState = Chasing } else e
      in e'  -- hitTimer increment handled in moveEnemy

    ShieldCharger ->
      -- Shield enemy always tries to rush
      e { aiState = Chasing }


-- Move all non-player entities.
moveEntities :: Float -> World -> World
moveEntities dt world =
  let p = player world
      run = currentRun world
      chamber = currentChamber run

      -- 1️⃣ Update all enemies (AI + movement + firing)
      (updatedEnemies, worldAfterEnemyUpdates) =
        foldl (\(accEnemies, w) e ->
                 let (e', w') = updateSingleEnemyAI dt p e w
                 in (accEnemies ++ [e'], w')
              ) ([], world) (enemies chamber)

      -- 2️⃣ Move all projectiles (use projectiles from updated world!)
      newChamberAfterEnemyUpdates = currentChamber (currentRun worldAfterEnemyUpdates)
      movedProjs = map (moveProjectile dt) (projectiles newChamberAfterEnemyUpdates)

      -- 3️⃣ Update chamber with new enemies and moved projectiles
      newChamber = newChamberAfterEnemyUpdates { enemies = updatedEnemies, projectiles = movedProjs }

  in worldAfterEnemyUpdates { currentRun = run { currentChamber = newChamber } }

handleTurretShoot :: Player -> ([Enemy], [Projectile]) -> Enemy -> ([Enemy], [Projectile])
handleTurretShoot p (enemiesAcc, projsAcc) e@(Enemy { enemyType = RangedTurret, enemyPos = (ex, ey), eBaseDmg = dmg, hitTimer = ht }) =
  if ht <= 0 then (e:enemiesAcc, projsAcc)
  else
    let (px, py) = playerPos p
        (dirX, dirY) = normalize (px - ex, py - ey)
        newProj = Projectile
          { projPos = (ex + dirX * enemyRadius e, ey + dirY * enemyRadius e)
          , projVel = (dirX * 200, dirY * 200)
          , projDmg = dmg
          , projSource = FromEnemy
          , projRadius = 5
          , projTTL = 5.0
          }
    in (e { hitTimer = 0 } : enemiesAcc, newProj : projsAcc)
handleTurretShoot _ (enemiesAcc, projsAcc) e = (e : enemiesAcc, projsAcc)


-- Move a single enemy.
-- Move a single enemy based on type and AI state
moveEnemy :: Float -> Player -> Enemy -> Enemy
moveEnemy dt p e =
  let (px, py) = playerPos p
      (ex, ey) = enemyPos e
      vecToPlayer = (px - ex, py - ey)
      (dirX, dirY) = normalize vecToPlayer
      speed = eBaseSpeed e
  in case enemyType e of

       MeleeBasic ->
         if aiState e == Chasing
         then e { enemyPos = (ex + dirX * speed * dt, ey + dirY * speed * dt) }
         else e

       RangedTurret ->
         let newPos = (ex + dirX * speed * 0.3 * dt, ey + dirY * speed * 0.3 * dt) -- slow wandering
             newHitTimer = hitTimer e + dt
         in if newHitTimer >= 4.0
            then e { hitTimer = 0, enemyPos = newPos }  -- fire projectile handled elsewhere
            else e { hitTimer = newHitTimer, enemyPos = newPos }

       ShieldCharger ->
         -- Move straight toward player
         e { enemyPos = (ex + dirX * speed * dt, ey + dirY * speed * dt) }
         
fireAtPlayer :: World -> Player -> Enemy -> (Enemy, World)
fireAtPlayer world p e =
  let (ex, ey) = enemyPos e
      (px, py) = playerPos p
      (dirX, dirY) = normalize (px - ex, py - ey)
      projectileSpeed = 900  
      newProj = Projectile
        { projPos    = (ex + dirX * enemyRadius e, ey + dirY * enemyRadius e)
        , projVel    = (dirX * projectileSpeed, dirY * projectileSpeed)
        , projDmg    = eBaseDmg e
        , projSource = FromEnemy
        , projRadius = 5
        , projTTL    = 5.0
        }
      run = currentRun world
      chamber = currentChamber run
      newChamber = chamber { projectiles = newProj : projectiles chamber }
      newRun = run { currentChamber = newChamber }
  in (e, world { currentRun = newRun })

-- Checks if player is in front of the shield during a charge
isFrontShielded :: Enemy -> Player -> Bool
isFrontShielded e p =
  enemyType e == ShieldCharger &&
  aiState e == Charging &&
  not (isHitFromBehind e (playerPos p))

isHitFromBehind :: Enemy -> (Float, Float) -> Bool
isHitFromBehind e (px, py) =
  let (ex, ey) = enemyPos e
      enemyfacingDir = enemyFacingDir e           -- use the actual field
      vecToPlayer = normalize (px - ex, py - ey)
      dotProd = fst enemyfacingDir * fst vecToPlayer + snd enemyfacingDir * snd vecToPlayer
  in dotProd < 0  -- True if player is behind

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
      gen = rng world
      (playerProjs, enemyProjs) = partitionProjs (projectiles chamber)
      allEnemies = enemies chamber

      -- Check player projectiles against enemies
      (survivingEnemies, hitProjs, newRewards, gen') = checkHits gen allEnemies playerProjs

      -- Check enemy projectiles against player
      (playerAfterHits, projsThatHitPlayer) = checkPlayerHit p pStats enemyProjs

      -- Filter out projectiles that hit something
      survivingPlayerProjs = filter (`notElem` hitProjs) playerProjs
      survivingEnemyProjs  = filter (`notElem` projsThatHitPlayer) enemyProjs

      -- Update chamber: new enemies, projectiles, and any rewards from kills
      newChamber = chamber
        { enemies = survivingEnemies
        , projectiles = survivingPlayerProjs ++ survivingEnemyProjs
        , rewards = rewards chamber ++ newRewards
        }

  in world { player = playerAfterHits, currentRun = run { currentChamber = newChamber }, rng = gen' }

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
-- Returns (surviving enemies, projectiles that hit)-- 1️⃣ Single projectile vs single enemy
-- Returns: (surviving enemies, whether projectile hit, new rewards, new RNG)
checkHit :: StdGen -> Projectile -> ([Enemy], Bool, [Reward]) -> Enemy -> ([Enemy], Bool, [Reward], StdGen)
checkHit gen proj (survivors, alreadyHit, rewards) enemy
  -- 1️⃣ Projectile already hit another enemy
  | alreadyHit = (survivors ++ [enemy], True, rewards, gen)

  -- 2️⃣ ShieldCharger ignores frontal hits while charging
  | enemyType enemy == ShieldCharger
    && aiState enemy == Charging
    && not (isHitFromBehind enemy (projPos proj)) =
      (survivors ++ [enemy], False, rewards, gen)

  -- 3️⃣ No collision
  | not collision = (survivors ++ [enemy], False, rewards, gen)

  -- 4️⃣ Enemy hit but still alive
  | newHealth > 0 = (survivors ++ [enemy { eCurrentHealth = newHealth }], True, rewards, gen)

  -- 5️⃣ Enemy dies
  | otherwise =
      let -- List of all possible boons to drop
          allBoons = --[ MoveSpeed 0.1
                        --, AttackSpeed 0.1
                        --, ExtraHealth 20
                        [ LongSword 12 ]
                        --, AttackDmg 1
                        --, DmgResist 0.1
                        --, ExtraDash 1]

          -- Randomly pick a boon
          (boonIndex, gen1) = randomR (0, length allBoons - 1) gen
          newBoon = allBoons !! boonIndex
--{
          -- 30% chance for a health drop
          (healthRoll, gen2) = randomR (0.0, 1.0 :: Float) gen1
          healthReward = if healthRoll <= 0.3
                         then [HealReward 15 (enemyPos enemy)]
                         else []
--}
          -- Combine rewards: boon + optional health
          newRewards = SimpleBoon newBoon (enemyPos enemy) : healthReward ++ rewards
      in (survivors, True, newRewards, gen2)
  where
    collision = isColliding (projPos proj) (projRadius proj) (enemyPos enemy) (enemyRadius enemy)
    newHealth = eCurrentHealth enemy - projDmg proj

-- 2️⃣ Single projectile applied to all enemies
applyProjectileToEnemies :: StdGen -> ([Enemy], [Projectile], [Reward]) -> Projectile -> ([Enemy], [Projectile], [Reward], StdGen)
applyProjectileToEnemies gen (enemies, hitProjs, rewards) proj =
    foldl applyOne ([], hitProjs, rewards, gen) enemies
  where
    applyOne (surv, hits, rws, g) enemy =
      let (newSurv, wasHit, newRws, g') = checkHit g proj (surv, False, rws) enemy
      in (newSurv, if wasHit then hits ++ [proj] else hits, newRws, g')

-- 3️⃣ Apply all projectiles to all enemies
checkHits :: StdGen -> [Enemy] -> [Projectile] -> ([Enemy], [Projectile], [Reward], StdGen)
checkHits gen enemies projs =
    foldl applyProj (enemies, [], [], gen) projs
  where
    applyProj (enemiesAcc, hitProjs, rewardsAcc, g) proj =
      applyProjectileToEnemies g (enemiesAcc, hitProjs, rewardsAcc) proj


-- Checks for player death and transitions state.
checkPlayerDeath :: World -> World
checkPlayerDeath world =
  if currentHealth (player world) <= 0
  then world { 
         gameState = GameOver,
         player = (player world) { 
             playerVel = (0,0), 
             facingDir = (0,1), 
             isDashing = False,
             dashTimer = 0
         },
         keys = (keys world) { keyW = False, keyA = False, keyS = False, keyD = False }
  }
  else world


-- Marks the chamber as cleared if all enemies are dead
checkRoomCleared :: World -> World
checkRoomCleared world =
  let run = currentRun world
      chamber = currentChamber run
  in if not (isCleared chamber) && null (enemies chamber)
     then
       let newChamber = chamber { isCleared = True }
           newRun = run { currentChamber = newChamber }
       in world { currentRun = newRun }
     else world

-- Handles player interaction (e.g., collecting rewards)
handleInteraction :: World -> World
handleInteraction world
  | keyInteract (keys world) =
      let p = player world
          run = currentRun world
          chamber = currentChamber run
          playerPos' = playerPos p
          radius = playerRadius

          (collected, remaining) = partition isCollidingWithPlayer (rewards chamber)
          world' = foldl (flip applyReward) world collected
          newChamber = chamber { rewards = remaining }
          newRun = run { currentChamber = newChamber }
      in world' { currentRun = newRun }
  | otherwise = world
  where
    isCollidingWithPlayer :: Reward -> Bool
    isCollidingWithPlayer r =
      let (rPos, rRad) = getRewardPosRad r
      in isColliding (playerPos (player world)) playerRadius rPos rRad

-- Helper to get position and radius from any Reward type.

-- Get position and radius of any reward
getRewardPosRad :: Reward -> ((Float, Float), Float)
getRewardPosRad (HealReward _ pos)       = (pos, healthDropRadius)
getRewardPosRad (SimpleBoon boon pos)    =
  case boon of
    ExtraHealth _ -> (pos, maxHealthBoonRadius)  -- bigger max health boon
    _             -> (pos, rewardRadius)        -- default for other boons
getRewardPosRad (CurrencyReward _ pos)   = (pos, rewardRadius)
getRewardPosRad (BoonChoice _ _ _)       = ((0, 100), rewardRadius)  -- screen-centered


-- Applies the collected reward to the player.
applyReward :: Reward -> World -> World
applyReward (HealReward amt _) world =
    let p       = player world
        pStats  = calculateStats p
        newHealth = min (statMaxHealth pStats) (currentHealth p + amt)
    in world { player = p { currentHealth = newHealth } }

applyReward (SimpleBoon boon _) world =
    let p        = player world
        newBoons = boon : currentBoons p
        updatedPlayer = case boon of
            ExtraHealth n ->
                p { baseMaxHealth = baseMaxHealth p + n
                  , currentBoons  = newBoons }
            MoveSpeed _ ->
                p { currentBoons = newBoons }
            AttackSpeed _ ->
                p { currentBoons = newBoons }
            DmgResist _ ->
                p { currentBoons = newBoons }  -- handled in calculateStats
            ExtraDash _ ->
                p { currentBoons = newBoons }  -- handled in calculateStats
            _ ->
                p { currentBoons = newBoons }
    in world { player = updatedPlayer }

applyReward (CurrencyReward amt _) world =
    let run = currentRun world
    in world { currentRun = run { runCurrency = runCurrency run + amt } }

applyReward (BoonChoice _ _ _) world =
    world { gameState = BoonSelection }
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

-- Vector helpers
addV :: (Float,Float) -> (Float,Float) -> (Float,Float)
addV (ax,ay) (bx,by) = (ax+bx, ay+by)

subV :: (Float,Float) -> (Float,Float) -> (Float,Float)
subV (ax,ay) (bx,by) = (ax-bx, ay-by)

scaleV :: Float -> (Float,Float) -> (Float,Float)
scaleV s (x,y) = (s*x, s*y)

rotateVectorTo :: (Float, Float) -> Picture -> Picture
rotateVectorTo (vx, vy) pic =
  rotate angle pic
  where
    angle = atan2 vy vx * 180 / pi   -- Gloss uses degrees