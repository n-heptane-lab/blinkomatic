{-# LANGUAGE Arrows, DeriveDataTypeable, RankNTypes #-}
module Blinkomatic where

import Control.Concurrent           (forkIO)
import Control.Concurrent.STM       (atomically)
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
import Pipes          ((>~), (>->), await, runEffect)
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

type MidiLights = (Fractional t, HasTime t s) => Wire s () Identity (Event MIDI) (Event [OutputEvent])

runShow :: (MonadIO m, Fractional t, HasTime t s) =>
           TMVar MIDI
        -> SerialPort
        -> Session m s
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

blinkomatic :: String -> MidiLights -> IO ()
blinkomatic midiDevice ml =
  do t <- atomically newEmptyTMVar
     mh <- openInput midiDevice None
     case mh of
       (Left e) ->
         do putStrLn =<< strError e
            exitFailure
       (Right h) ->
         do putStrLn "opened MPD32 MIDI Input."
            forkIO $ runEffect $ (lift $ RawMidi.read h) >~ parseMidi >-> (forever $ do m <- await ; lift $ atomically $ putTMVar t m)
            s <- openSerial port defaultSerialSettings { commSpeed = baud }
            runShow t s clockSession_ ml

red' :: MidiLights
red' =
  for 1 . (now . pure [SendCmd $ Command red Open])  -->
  for 1 . (now . pure [SendCmd $ Command red Close]) -->
  red'

yellow' :: MidiLights
yellow' =
  for 0.5 . (now . pure [SendCmd $ Command yellow Open])  -->
  for 0.5 . (now . pure [SendCmd $ Command yellow Close]) -->
  yellow'

rest' :: MidiLights
rest' =
  for 0.25 . (now . pure (map (\c -> SendCmd $ Command c Open)  [darkOrange, lightOrange, green, blue, violet, white])) -->
  for 0.25 . (now . pure (map (\c -> SendCmd $ Command c Close) [darkOrange, lightOrange, green, blue, violet, white])) -->
  rest'

multi :: MidiLights
multi =
  proc e ->
  do r <- red'    -< e
     y <- yellow' -< e
     r' <- rest'   -< e
     returnA -< merge (++) r' (merge (++) r y)

chase :: MidiLights
chase =
  let period = 0.25 in
  for period . (now . pure [SendCmd $ Command red         Close, SendCmd $ Command white       Open]) -->
  for period . (now . pure [SendCmd $ Command darkOrange  Close, SendCmd $ Command red         Open]) -->
  for period . (now . pure [SendCmd $ Command lightOrange Close, SendCmd $ Command darkOrange  Open]) -->
  for period . (now . pure [SendCmd $ Command yellow      Close, SendCmd $ Command lightOrange Open]) -->
  for period . (now . pure [SendCmd $ Command green       Close, SendCmd $ Command yellow      Open]) -->
  for period . (now . pure [SendCmd $ Command blue        Close, SendCmd $ Command green       Open]) -->
  for period . (now . pure [SendCmd $ Command violet      Close, SendCmd $ Command blue        Open]) -->
  for period . (now . pure [SendCmd $ Command white       Close, SendCmd $ Command violet      Open]) -->
  chase

showMidi :: MidiLights
showMidi =
  accumE (\_ m -> [Print $ show m]) []
