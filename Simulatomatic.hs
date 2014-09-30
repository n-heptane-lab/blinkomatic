{-# LANGUAGE RankNTypes #-}
module Main where

import Blinkomatic
import Color (RGB(..), blackRGB, rgb_w2d)
import Control.Exception (bracket)
import Control.Monad          (forM_)
import Control.Monad.Identity (Identity(..))
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (MonadIO(liftIO), lift)
import Control.Wire (Wire, Event, Session(..), Timed(..), countSession_, stepWire, stepSession)
import Control.Concurrent.STM      (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Wire.Unsafe.Event
import Data.Time.Clock
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.Maybe  (fromJust)
import Data.Vector                   (Vector)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import Data.Word (Word8)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import Sound.NH.MIDI.Core (MIDI(MidiClock))

data Scene = Scene
  { elWires   :: MV.IOVector Switch
  , tclLights :: TVar (Vector (RGB Word8))
  }

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

drawScene :: (MonadIO m) => GLFW.Window -> Scene -> m ()
drawScene window scene = liftIO $
  do GL.clear [GL.ColorBuffer, GL.DepthBuffer]
     GL.loadIdentity
     GL.translate $ GL.Vector3 (-20) 0 (-50 :: GL.GLfloat)
     -- el wire
     GL.renderPrimitive GL.Quads $
       forM_ [0..7] $ \i -> do
         switch <- liftIO $ MV.read (elWires scene) (fromIntegral i)
         forM_ [(0,0), (1,0), (1,1), (0, 1)] $ \(x, y) ->
           let vtx = GL.Vertex3 (x + (fromIntegral i * 2)) y 0 :: GL.Vertex3 GL.GLfloat
           in do case switch of
                     Open -> GL.color (GL.Color3 0.1 0.1 (0.1 :: GL.GLfloat))
                     Close  -> GL.color $ fromJust $ IntMap.lookup i colorMap
                 GL.vertex vtx
     -- tcl
     tcl <- atomically $ readTVar $ tclLights scene
     GL.renderPrimitive GL.Quads $
       forM_ [0..24::Int] $ \i -> do
         let light = tcl V.! (fromIntegral i)
         forM_ [(0,0), (1,0), (1,1), (0, 1)] $ \(x, y) ->
           let vtx = GL.Vertex3 (x + (fromIntegral i * 2)) (y+10) 0 :: GL.Vertex3 GL.GLfloat
           in do let RGB r g b = rgb_w2d light
                 GL.color (GL.Color3 (realToFrac r) (realToFrac g) (realToFrac b :: GL.GLfloat))
                 GL.vertex vtx
     printErrors

     GL.flush
     GLFW.swapBuffers window

simulateShow :: (MonadIO m) =>
                GLFW.Window
             -> Scene
             -> Session m (Timed Int ())
             -> MidiLights
             -> m a
simulateShow window scene s0 w0 = loop s0 w0
  where
    loop s' w' = do
      (ds, s) <- stepSession s'
      let Identity (mx, w) =  stepWire w' ds (Right NoEvent)
      case mx of
        (Right (Event actions)) ->
          do let perform (Print s)     = liftIO $ putStrLn s
                 perform (SendCmd (Command channel switch)) =
                   liftIO $ MV.write (elWires scene) (fromIntegral channel) switch
                 perform (TCL tcl) = liftIO $ atomically $ writeTVar (tclLights scene) tcl
             mapM_ perform actions
             liftIO $ GLFW.pollEvents
             drawScene window scene
        _                 -> return ()
      loop s w

simulate :: Double -> MidiLights -> IO ()
simulate bpm midiLights =
  bracket GLFW.init (const GLFW.terminate) $ \True -> do
    (Just window) <- GLFW.createWindow 640 480 "blinkomatic" Nothing Nothing
    GLFW.makeContextCurrent (Just window)
    GLFW.setWindowSizeCallback window (Just resize)
    GL.depthFunc $= Just GL.Less
    el  <- MV.replicate 10 Open
    tcl <- atomically $ newTVar $ V.replicate 25 blackRGB
    simulateShow window (Scene el tcl) (fakeMidiClock bpm) midiLights
    return ()

fakeMidiClock :: (MonadIO m) => Double -> MidiSession m
fakeMidiClock bpm =
  Session $ do
    t0 <- liftIO getCurrentTime
    return (Timed 0 (), loop t0)
    where
      secondsPerPulse :: NominalDiffTime
      secondsPerPulse = ((1 / (realToFrac bpm)) * 60) / 24
      loop t' =
        Session $ do
          t <- liftIO getCurrentTime
          let dt = diffUTCTime t t'
              dmc :: Int
              dmc = floor (realToFrac (dt / secondsPerPulse) :: Double)
          if dt > secondsPerPulse
            then return (Timed dmc (), loop (addUTCTime ((fromIntegral dmc) * secondsPerPulse) t'))
            else return (Timed 0 (), loop t')

main :: IO ()
main = simulate 120 tclFade
