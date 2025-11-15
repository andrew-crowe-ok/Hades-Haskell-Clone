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
    , keyAttack   :: Bool
    , keyMelee    :: Bool
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
  { enemies     :: [Enemy]
  , projectiles :: [Projectile]
  , reward      :: Maybe Reward
  , isCleared   :: Bool
  } deriving (Show, Read)


data Enemy = Enemy
  { enemyPos       :: (Float, Float)
  , eCurrentHealth :: Int
  , eBaseHealth    :: Int
  , eBaseSpeed     :: Float
  , eBaseDmg       :: Int
  , enemyType      :: EnemyType
  , aiState        :: AiState
  , enemyRadius    :: Float
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
    } deriving (Show, Read)


data WeaponType = Sword | Bow
    deriving (Show, Read, Eq)


data Boon
    = AttackDmg Int
    | AttackSpeed Float
    | ExtraHealth Int
    | MoveSpeed Float
    | DmgResist Float
    | ExtraDash Int
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