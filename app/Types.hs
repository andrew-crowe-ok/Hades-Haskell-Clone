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
  { -- currentScene :: Scene -- DELETED. We will use gameState as the scene manager.
    gameState    :: GameState
  , player       :: Player
  , currentRun   :: RunState
  , metaProgress :: MetaProgress
  , rng          :: StdGen
  , keys         :: KeyState
  , worldTime    :: Float -- ADDED: Tracks total elapsed time for cooldowns
  } deriving (Show)


data Player = Player
  { playerPos        :: (Float, Float)
  , playerVel        :: (Float, Float)
  , currentHealth    :: Int -- RENAMED from playerHealth
  , baseMaxHealth    :: Int -- RENAMED from maxHealth
  , baseSpeed        :: Float -- ADDED: Base speed before boons
  , baseDmgResist :: Float -- ADDED: Base resist (0.0 = 0%)
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
  , statDmgResist :: Float
  , statAttackDmg :: Int
  , statAttackRate   :: Float
  , statDashCount    :: Int
  } deriving (Show, Read)


data KeyState = KeyState
    { keyW :: Bool
    , keyA :: Bool
    , keyS :: Bool
    , keyD :: Bool
    , keyAttack :: Bool -- ADDED
    , keyDash   :: Bool -- ADDED
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
  { enemies     :: [Enemy]
  , projectiles :: [Projectile]
  , reward      :: Maybe Reward
  , isCleared   :: Bool
  } deriving (Show, Read)


data Enemy = Enemy
  { enemyPos      :: (Float, Float)
  -- Use eCurrentHealth to avoid name clash with player's currentHealth
  , eCurrentHealth :: Int -- RENAMED from enemyHealth
  , eBaseHealth    :: Int -- ADDED
  , eBaseSpeed     :: Float -- ADDED
  , eBaseDmg    :: Int -- ADDED
  , enemyType     :: EnemyType
  , aiState       :: AiState
  , enemyRadius   :: Float
  } deriving (Show, Read)


data EnemyType
    = MeleeBasic
    | RangedTurrent
    deriving (Show, Read, Eq)


data AiState
    = Idle
    | Chasing
    | Attacking
    deriving (Show, Read)


data Projectile = Projectile
    { projPos    :: (Float, Float)
    , projVel    :: (Float, Float)
    , projDmg :: Int
    , projSource :: ProjectileSource
    , projRadius :: Float
    , projTTL    :: Float -- Time to Live (seconds)
    } deriving (Show, Read, Eq)


data ProjectileSource = FromPlayer | FromEnemy
    deriving (Show, Read, Eq)


data Weapon = Weapon
    { weaponType   :: WeaponType
    , baseDmg   :: Int -- RENAMED from damage
    , baseAttackRate :: Float -- RENAMED from attackRate
    , lastAttack   :: Float
    } deriving (Show, Read)


data WeaponType = Sword | Bow
    deriving (Show, Read, Eq)


data Boon
    = AttackDmg Int
    | AttackSpeed Float
    | ExtraHealth Int
    | MoveSpeed Float     -- ADDED: Multiplier
    | DmgResist Float  -- ADDED: Additive (0.1 = 10%)
    | ExtraDash Int       -- ADDED: Additive
    deriving (Show, Read)


data Reward
    = Heal Int
    | BoonChoice Boon Boon Boon
    | Currency Int
    deriving (Show, Read)


data MetaUpgrade
    = StartWithMoreHealth
    | UnlockWeapon WeaponType
    deriving (Show, Read)

-- DELETED: MenuState, GameOverState, and Scene
-- These are no longer needed as 'GameState' handles everything.