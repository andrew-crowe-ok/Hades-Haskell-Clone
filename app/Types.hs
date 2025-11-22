module Types where

import System.Random (StdGen)


data GameState
  = MainMenu
  | Running
  | Paused
  | BoonSelection
  | GameOver
  | HubWorld
  deriving (Show, Read, Eq)  


data World = World
  { gameState    :: GameState
  , player       :: Player
  , currentRun   :: RunState
  , metaProgress :: MetaProgress
  , rng          :: StdGen
  , mousePos :: (Float, Float)  -- store current mouse position
  , enemySpawnTimer :: Float
  , keys         :: KeyState
  , worldTime    :: Float
  } deriving (Show)


data Player = Player
  { playerPos        :: (Float, Float)
  , playerVel        :: (Float, Float)
  , currentHealth    :: Int
  , baseMaxHealth    :: Int
  , baseSpeed        :: Float
  , baseDmgResist    :: Float
  , currentWeapon    :: Weapon
  , currentBoons     :: [Boon]
  , facingDir        :: (Float, Float)
  , dashCount        :: Int
  , dashCooldown     :: Float
  , dashTimer        :: Float
  , isDashing        :: Bool
  } deriving (Show, Read)

-- | Holds the *final, calculated* stats after boons are applied.
data PlayerStats = PlayerStats
  { statMaxHealth    :: Int
  , statSpeed        :: Float
  , statDmgResist    :: Float
  , statAttackDmg    :: Int
  , statAttackRate   :: Float
  , statDashCount    :: Int
  } deriving (Show, Read)


data KeyState = KeyState
    { keyW        :: Bool
    , keyA        :: Bool
    , keyS        :: Bool
    , keyD        :: Bool
    , keyMelee    :: Bool
    , keyAttack  :: Bool 
    , keyDash     :: Bool
    , keyInteract :: Bool
    } deriving (Show, Read)


data RunState = RunState
  { currentChamber :: Chamber
  , chamberLevel   :: Int
  , runCurrency    :: Int
  } deriving (Show, Read)


data MetaProgress = MetaProgress
  { unlockedWeapons   :: [WeaponType]
  , permanentUpgrades :: [MetaUpgrade]
  , metaCurrency      :: Int
  } deriving (Show, Read)


data Chamber = Chamber
  { enemies   :: [Enemy]
  , projectiles :: [Projectile]
  , isCleared :: Bool
  , rewards   :: [Reward]  
  } deriving (Show, Read)

data Enemy = Enemy
  { enemyPos       :: (Float, Float)
  , eCurrentHealth :: Int
  , eBaseHealth    :: Int
  , eBaseSpeed     :: Float
  , eBaseDmg       :: Int
  , enemyType      :: EnemyType
  , aiState        :: AiState
  , chargeTimer    :: Float  
  , enemyRadius    :: Float
  , fireTimer       :: Float       -- NEW: countdown to next shot
  , hitTimer       :: Float      -- cooldown for attacks or hit invulnerability
  , enemyFacingDir      :: (Float, Float)  -- needed for ShieldCharger
  } deriving (Show, Read)

data EnemyType
    = MeleeBasic
    | RangedTurret
    | ShieldCharger    -- Shield enemy, vulnerable from behind
    deriving (Show, Read, Eq)


data AiState
    = Idle
    | Chasing
    | Charging 
    | Recovering
    | Attacking
    deriving (Show, Read, Eq)


data Projectile = Projectile
    { projPos    :: (Float, Float)
    , projVel    :: (Float, Float)
    , projDmg    :: Int
    , projSource :: ProjectileSource
    , projRadius :: Float
    , projTTL    :: Float
    } deriving (Show, Read, Eq)


data ProjectileSource = FromPlayer | FromEnemy
    deriving (Show, Read, Eq)


data Weapon = Weapon
    { weaponType     :: WeaponType
    , baseDmg        :: Int
    , baseAttackRate :: Float
    , lastAttack     :: Float
    , lastMeleeAttack :: Float   -- For melee attacks

    } deriving (Show, Read)


data WeaponType = Sword | Bow
    deriving (Show, Read, Eq)


data Boon
    = AttackDmg Int           -- General damage upgrade to both sword and projectiles
    | AttackSpeed Float        -- faster shooting
    | ExtraHealth Int       --  more health  (visual health reflects this.)
    | MoveSpeed Float                 
    | DmgResist Float           -- take less damage on hit
    | ExtraDash Int
    | LongSword Int               -- +damage, longer melee range
    | MultiShot Int               -- number of extra projectiles per shot
    | RapidFire Float             -- low famage, medium speed 
    | SniperShot Int Float        -- more damage, faster projectile
    | RotatingShield Float Float  -- shield radius, cooldown/regen time
    deriving (Show, Read)


data Reward
    = HealReward Int (Float, Float)
    | SimpleBoon Boon (Float, Float)
    | BoonChoice Boon Boon Boon
    | CurrencyReward Int (Float, Float)
    deriving (Show, Read)


data MetaUpgrade
    = StartWithMoreHealth
    | UnlockWeapon WeaponType
    deriving (Show, Read)