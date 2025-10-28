module Constants where

import Brillo

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