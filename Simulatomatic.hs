{-# LANGUAGE RankNTypes #-}
module Main where

import Blinkomatic
import Control.Exception (bracket)
import Control.Monad          (forM_)
import Control.Monad.Identity (Identity(..))
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (MonadIO(liftIO), lift)
import Control.Wire (Wire, Event, Session, Timed(..), countSession_, stepWire, stepSession)
import Control.Wire.Unsafe.Event
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.Maybe  (fromJust)
import qualified Data.Vector.Mutable as V
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

colorMap :: IntMap (GL.Color3 GL.GLfloat)
colorMap = IntMap.fromList
  [ (0, GL.Color3 1.0 0.0 0.0) -- red
  , (1, GL.Color3 1.0 0.5 0.0) -- dark orange
  , (2, GL.Color3 1.0 0.8 0.0) -- light orange
  , (3, GL.Color3 1.0 1.0 0.0) -- yellow
  , (4, GL.Color3 0.5 1.0 0.0) -- light green
  , (5, GL.Color3 0.0 0.0 1.0) -- blue
  , (6, GL.Color3 0.7 0.7 0.9) -- violet
  , (7, GL.Color3 1.0 1.0 1.0) -- white
  ]

-- | Resize the viewport and set the projection matrix
resize window w h = do
  -- These are all analogous to the standard OpenGL functions
  GL.viewport     $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode   $= GL.Projection
  GL.loadIdentity
  GL.perspective  45 (fromIntegral w / fromIntegral h) 1 100
  GL.matrixMode   $= GL.Modelview 0

-- | This will print and clear the OpenGL errors
printErrors = GL.get GL.errors >>= mapM_ print

drawLights :: (MonadIO m) => GLFW.Window -> V.IOVector Switch -> m ()
drawLights window v = liftIO $
  do GL.clear [GL.ColorBuffer, GL.DepthBuffer]
     GL.loadIdentity
     GL.translate $ GL.Vector3 (-10) 0 (-50 :: GL.GLfloat)
     GL.renderPrimitive GL.Quads $
       forM_ [0..7] $ \i -> do
         switch <- liftIO $ V.read v (fromIntegral i)
         forM_ [(0,0), (1,0), (1,1), (0, 1)] $ \(x, y) ->
           let vtx = GL.Vertex3 (x + (fromIntegral i * 2)) y 0 :: GL.Vertex3 GL.GLfloat
           in do case switch of
                     Open -> GL.color (GL.Color3 0.1 0.1 (0.1 :: GL.GLfloat))
                     Close  -> GL.color $ fromJust $ IntMap.lookup i colorMap
                 GL.vertex vtx
     printErrors

     GL.flush
     GLFW.swapBuffers window

simulateShow :: (MonadIO m) =>
                GLFW.Window
             -> V.IOVector Switch
             -> Session m (Timed Int ())
             -> MidiLights
             -> m a
simulateShow window v s0 w0 = loop s0 w0
  where
    loop s' w' = do
      (ds, s) <- stepSession s'
      let Identity (mx, w) =  stepWire w' ds (Right NoEvent)
      case mx of
        (Right (Event actions)) ->
          do let perform (Print s)     = liftIO $ putStrLn s
                 perform (SendCmd (Command channel switch)) =
                   liftIO $ V.write v (fromIntegral channel) switch
             mapM_ perform actions
        _                 -> return ()
      liftIO $ GLFW.pollEvents
      drawLights window v
      loop s w

simulate :: MidiLights -> IO ()
simulate midiLights =
  bracket GLFW.init (const GLFW.terminate) $ \True -> do
    (Just window) <- GLFW.createWindow 640 480 "blinkomatic" Nothing Nothing
    GLFW.makeContextCurrent (Just window)
    GLFW.setWindowSizeCallback window (Just resize)
    GL.depthFunc $= Just GL.Less
    v <- V.replicate 10 Open
    simulateShow window v (countSession_ 1) midiLights
    return ()

main :: IO ()
main = simulate kickSnare
