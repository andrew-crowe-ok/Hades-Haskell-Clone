module Constants where

import Graphics.Gloss

windowWidth :: Int
windowWidth = 800

windowHeight :: Int
windowHeight = 600

window :: Display
window = InWindow "Project Tartarus" (windowWidth, windowHeight) (100, 100)

bgColor :: Color
bgColor = black

fps :: Int
fps = 60

roomWidth :: Float
roomWidth = 750

roomHeight :: Float
roomHeight = 550

playerSpeed :: Float
playerSpeed = 200

playerRadius :: Float
playerRadius = 10

defaultEnemyRadius :: Float
defaultEnemyRadius = 20

projectileRadius :: Float
projectileRadius = 5

projectileSpeed :: Float
projectileSpeed = 400

projectileTTL :: Float -- Time to Live
projectileTTL = 2.0 -- seconds

dashSpeed :: Float
dashSpeed = 400  -- faster than normal playerSpeed

healthDropRadius :: Float
healthDropRadius = 10     -- smaller heal pickup

maxHealthBoonRadius :: Float
maxHealthBoonRadius = 16  -- larger max health boon

-- | Dash duration in seconds
dashDuration :: Float
dashDuration = 0.1

-- | Multiplier for dash speed (how much faster than normal)
dashMultiplier :: Float
dashMultiplier = 3.5

-- | Maximum dashes player can hold
maxDash :: Int
maxDash = 3

rewardRadius :: Float -- ADDED
rewardRadius = 15