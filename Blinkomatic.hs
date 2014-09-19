{-# LANGUAGE Arrows, DeriveDataTypeable, RankNTypes #-}
module Blinkomatic where

import Control.Arrow
import Control.Concurrent           (forkIO)
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TVar  (TVar, newTVarIO, readTVarIO, writeTVar, modifyTVar')
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, putTMVar, tryTakeTMVar)
import Control.Monad                (forever)
import Control.Monad.Trans          (lift)
import Control.Wire
import Control.Wire.Unsafe.Event
import Control.Monad.Identity (Identity(..))
import Control.Monad.Trans (MonadIO(..))
import Data.Bits           ((.|.), shiftL)
import Data.ByteString     (singleton)
import Data.Data (Data, Typeable)
import Data.Word (Word8)
import Pipes          ((>~), (>->), Consumer, await, runEffect)
import Prelude hiding ((.))
import Sound.NH.MIDI.Core
import Sound.NH.MIDI.Parse   (parseMidi)
import Sound.NH.ALSA.RawMidi as RawMidi (RawMode(None), openInput, read, strError)
import System.Hardware.Serialport (CommSpeed(CS9600), SerialPort, SerialPortSettings(commSpeed), defaultSerialSettings, openSerial, send)
import System.Exit (exitFailure)

port = "/dev/ttyUSB0"
baud = CS9600

data Switch
  = Open
  | Close
    deriving (Eq, Ord, Read, Show, Data, Typeable)

s2b :: Switch -> Bool
s2b Close = True
s2b Open  = False

red, darkOrange, lightOrange, yellow, green, blue, violet, white :: Word8
red         = 0
darkOrange  = 1
lightOrange = 2
yellow      = 3
green       = 4
blue        = 5
violet      = 6
white       = 7

data OutputEvent
  = SendCmd Command
  | Print String

data Command = Command
    { channel :: Word8
    , switch  :: Switch
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

serializeCommand :: Command -> Word8
serializeCommand (Command channel switch) =
  (channel `shiftL` 1) .|. (case switch of
                               Open  -> 0
                               Close -> 1)

type MidiWire   = Wire (Timed Int ()) () Identity
type MidiLights = Wire (Timed Int ()) () Identity (Event MIDI) (Event [OutputEvent])

beatSession :: (MonadIO m) =>
               TVar Int
            -> Session m (Timed Int ())
beatSession pos =
  Session $ do
    p0 <- liftIO $ readTVarIO pos
    return (Timed 0 (), loop p0)
  where
    loop p' =
      Session $ do
        p <- liftIO $ readTVarIO pos
        let dp = p - p'
        dp `seq` return (Timed dp (), loop p)

runShow :: (MonadIO m) =>
           TMVar MIDI
        -> SerialPort
        -> Session m (Timed Int ())
        -> MidiLights
        -> m a
runShow midi serialPort s0 w0 = loop s0 w0
  where
    loop s' w' = do
      (ds, s) <- stepSession s'
      mm      <- liftIO $ atomically $ tryTakeTMVar midi
      let Identity (mx, w) = stepWire w' ds (Right $ case mm of
                                                       Nothing -> NoEvent
                                                       (Just m) -> Event m
                                            )
      case mx of
        (Right (Event actions)) ->
          do let perform (Print s)     = liftIO $ putStrLn s
                 perform (SendCmd cmd) = liftIO $ send serialPort (singleton $ serializeCommand cmd) >> return ()
             mapM_ perform actions
        _                 -> return ()
      loop s w

midiConsumer :: TVar Int
             -> TMVar MIDI
             -> Consumer MIDI IO ()
midiConsumer pos t =
   forever $ do
     m <- await
     lift $ atomically $ putTMVar t m
     case m of
       SongPositionPointer spp ->
         lift $ print m
---         lift $ atomically $ modifyTVar' pos succ
--         lift $ atomically $ writeTVar pos (fromIntegral $ _value14 spp)
       MidiClock -> do
         lift $ atomically $ modifyTVar' pos succ
--         lift $ print =<< readTVarIO pos
       _ -> lift $ print m

blinkomatic :: String -> MidiLights -> IO ()
blinkomatic midiDevice ml =
  do t   <- atomically newEmptyTMVar
     pos <- newTVarIO 0
     mh <- openInput midiDevice None
     case mh of
       (Left e) ->
         do putStrLn =<< strError e
            exitFailure
       (Right h) ->
         do putStrLn "opened MPD32 MIDI Input."
            forkIO $ runEffect $ (lift $ RawMidi.read h) >~ parseMidi >-> midiConsumer pos t
            s <- openSerial port defaultSerialSettings { commSpeed = baud }
            runShow t s (beatSession pos) ml


atMod :: (HasTime t s, Integral t) =>
         t
      -> t  -- ^ Time of occurrence.
      -> Wire s e m a (Event a)
atMod m t' =
    mkSF $ \ds x ->
        let t = t' - ((dtime ds) `mod` m)
        in if t <= 0
             then (Event x, never)
             else (NoEvent, atMod m t')

red' :: MidiLights
red' =
  for 1 . (now . pure [SendCmd $ Command red Open])  -->
  for 1 . (now . pure [SendCmd $ Command red Close]) -->
  red'

yellow' :: MidiLights
yellow' =
  for 5 . (now . pure [SendCmd $ Command yellow Open])  -->
  for 5 . (now . pure [SendCmd $ Command yellow Close]) -->
  yellow'

rest' :: MidiLights
rest' =
  for 25 . (now . pure (map (\c -> SendCmd $ Command c Open)  [darkOrange, lightOrange, green, blue, violet, white])) -->
  for 25 . (now . pure (map (\c -> SendCmd $ Command c Close) [darkOrange, lightOrange, green, blue, violet, white])) -->
  rest'

multi :: MidiLights
multi =
  proc e ->
  do r <- red'    -< e
     y <- yellow' -< e
     r' <- rest'   -< e
     returnA -< merge (++) r' (merge (++) r y)

sixteenth, eighth, quarter :: Int
sixteenth = 6
eighth    = 12
quarter   = 24
half      = 48

chase :: MidiLights
chase =
  let period = quarter in
  for period . (now . pure [SendCmd $ Command red         Close, SendCmd $ Command white       Open]) -->
  for period . (now . pure [SendCmd $ Command darkOrange  Close, SendCmd $ Command red         Open]) -->
  for period . (now . pure [SendCmd $ Command lightOrange Close, SendCmd $ Command darkOrange  Open]) -->
  for period . (now . pure [SendCmd $ Command yellow      Close, SendCmd $ Command lightOrange Open]) -->
  for period . (now . pure [SendCmd $ Command green       Close, SendCmd $ Command yellow      Open]) -->
  for period . (now . pure [SendCmd $ Command blue        Close, SendCmd $ Command green       Open]) -->
  for period . (now . pure [SendCmd $ Command violet      Close, SendCmd $ Command blue        Open]) -->
  for period . (now . pure [SendCmd $ Command white       Close, SendCmd $ Command violet      Open]) -->
  chase

kickSnare :: MidiLights
kickSnare =
  let period = quarter
      right  = [ SendCmd $ Command red         Close
               , SendCmd $ Command darkOrange  Close
               , SendCmd $ Command lightOrange Close
               , SendCmd $ Command yellow      Close
               , SendCmd $ Command green       Open
               , SendCmd $ Command blue        Open
               , SendCmd $ Command violet      Open
               , SendCmd $ Command white       Open
               ]
      left = [ SendCmd $ Command red         Open
             , SendCmd $ Command darkOrange  Open
             , SendCmd $ Command lightOrange Open
             , SendCmd $ Command yellow      Open
             , SendCmd $ Command green       Close
             , SendCmd $ Command blue        Close
             , SendCmd $ Command violet      Close
             , SendCmd $ Command white       Close
             ]
  in
   (periodic half . pure right) <> (periodic half . after quarter . pure left)
--   (periodic half . pure [Print "boo"]) <> (periodic half . after quarter . pure [Print "urns"])
--     (at 0       . (pure right))  --> (now . pure [Print "Boo"])
--     (at 5 . (pure left)) -- -->
--     kickSnare
{- -}
-- firstBeat :: MidiWire Int (Event Int)
-- firstBeat = became (\t -> (t :: Int) `mod` (24 * 2) < 24) . time



--  time . when (\t -> t `mod` (24 * 2) < 24)
  {-
  for period . (now . pure [ SendCmd $ Command red         Close
                           , SendCmd $ Command darkOrange  Close
                           , SendCmd $ Command lightOrange Close
                           , SendCmd $ Command yellow      Close
                           , SendCmd $ Command green       Open
                           , SendCmd $ Command blue        Open
                           , SendCmd $ Command violet      Open
                           , SendCmd $ Command white       Open
                           ]) -->
  for period . (now . pure [ SendCmd $ Command red         Open
                           , SendCmd $ Command darkOrange  Open
                           , SendCmd $ Command lightOrange Open
                           , SendCmd $ Command yellow      Open
                           , SendCmd $ Command green       Close
                           , SendCmd $ Command blue        Close
                           , SendCmd $ Command violet      Close
                           , SendCmd $ Command white       Close
                           ]) -->
  kickSnare
-}

showMidi :: MidiLights
showMidi =
  accumE (\_ m -> [Print $ show m]) []
