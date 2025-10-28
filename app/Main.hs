module Main where

import Brillo
import Brillo.Interface.Pure.Game
import Types
import Constants


initialWorld :: World
initalWorld world =


drawWorld :: World -> Picture
drawWorld world =
  case currentScene world of
    SceneMenu menuState          ->  drawMenu menuState
    SceneGame gameState          ->  drawGame gameState
    SceneGameOver gameOverState  ->  drawGameOver gameOverState


drawMenu :: MenuState -> Picture
drawMenu state =


drawGame :: GameState -> Picture
drawGame state =


drawGameOver :: GameOverState -> Picture
drawGameOver state =


handleEvent :: Event -> World -> World
handleEvent event world =
  case currentScene world of
    
    SceneMenu menuState ->
      handleMenuInput even menuState world
    
    SceneGame gameState ->
      handleGameInput event gameState world

    SceneGameOver gameOverState -> 
      handleGameOverInput event gameOverState world


updateWorld :: Float -> World -> World
updateWorld secondsPassed world =
  case currentScene world of
    SceneMenu     _  ->  world
    SceneGameOver _  ->  world

    SceneGame gameState ->
      let newGameState = updateGame secondsPassed gameState
      in world
        currentScene = SceneGame newGameState

main :: IO ()
main =
  play
    (InWindow "Moving Object" (windowWidth, windowHeight) (10, 10)) -- Window
    white                                 -- Background color
    60                                    -- Simulation steps per second
    initialWorld                          -- Initial world state
    drawWorld                             -- Function to draw the world
    handleEvent                           -- Function to handle events
    updateWorld