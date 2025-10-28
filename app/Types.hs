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
  { currentScene :: Scene
  , gameState    :: GameState
  , player       :: Player
  , currentRun   :: RunState
  , metaProgress :: MetaProgress
  , rng          :: StdGen
  , keys         :: KeyState
  }


data Player = Player
  { playerPos     :: (Float, Float)
  , playerVel     :: (Float, Float)
  , playerHealth  :: Int
  , maxHealth     :: Int
  , currentWeapon :: Weapon
  , currentBoons  :: [Boon]
  --, dashCharges   :: Int
  --, lastDashTime  :: Float
  } deriving (Show, Read)


data KeyState = KeyState
    { keyW :: Bool
    , keyA :: Bool
    , keyS :: Bool
    , keyD :: Bool
    }


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
  --, layout      :: ChamberLayout
  , reward      :: Maybe Reward
  , isCleared   :: Bool
  } deriving (Show, Read)


data Enemy = Enemy
  { enemyPos    :: (Float, Float)
  , enemyHealth :: Int
  , enemyType   :: EnemyType
  , aiState     :: AiState
  , enemyRadius :: Float
  } deriving (Show, Read)


data EnemyType
    = MeleeBasic
    | RangedTurrent
    deriving (Show, Read, Eq)


data AiState
    = Idle
    | Chasing Player
    | Attacking
    deriving (Show, Read)


data Projectile = Projectile
    { projPos    :: (Float, Float)
    , projVel    :: (Float, Float)
    , projDamage :: Int
    , projSource :: ProjectileSource
    , projRadius :: Float
    , projTTL    :: Float -- Time to Live (seconds)
    } deriving (Show, Read)


data ProjectileSource = FromPlayer | FromEnemy
    deriving (Show, Read)


data Weapon = Weapon
    { weaponType :: WeaponType
    , damage     :: Int
    , attackRate :: Float
    , lastAttack :: Float
    } deriving (Show, Read)


data WeaponType = Sword | Bow
    deriving (Show, Read, Eq)


data Boon
    = AttackDamage Int
    | AttackSpeed Float
    | ExtraHealth Int
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


data MenuState = MenuState
  { selectedOption :: Int
  } deriving (Show, Read)


data GameOverState = GameOverState
  { finalScore :: Int
  } deriving (Show, Read)


data Scene 
    = SceneMenu MenuState 
    | SceneGame GameState 
    | SceneGameOver GameOverState
    deriving (Show, Read)
