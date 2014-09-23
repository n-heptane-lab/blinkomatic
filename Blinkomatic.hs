{-# LANGUAGE Arrows, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
module Blinkomatic where

import Control.Arrow
-- import Control.Applicative
import Control.Concurrent           (forkIO)
import Control.Concurrent.STM       (atomically)
-- import Control.Concurrent.STM.TVar  (TVar, newTVarIO, readTVarIO, writeTVar, modifyTVar')
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, putTMVar, takeTMVar, tryTakeTMVar)
-- import Control.Concurrent.STM.SSem  (SSem, new, waitN', signal)
import Control.Monad                (forever, msum)
import Control.Monad.Trans          (lift)
import Control.Wire
import Control.Wire.Core
import Control.Wire.Unsafe.Event
import Control.Monad.Identity (Identity(..))
import Control.Monad.Trans (MonadIO(..))
import Data.Bits           ((.|.), shiftL)
import Data.ByteString     (singleton)
import Data.Data (Data, Typeable)
import Data.Foldable       (asum)
import Data.Word (Word8)
import Debug.Trace (trace)
import Pipes          ((>~), (>->), Consumer, await, runEffect)
import Prelude hiding ((.), id)
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

red, darkOrange, lightOrange, yellow, green, blue, violet, white, whiteStrip :: Word8
red         = 0
darkOrange  = 1
lightOrange = 2
yellow      = 3
green       = 4
blue        = 5
violet      = 6
white       = 7
whiteStrip  = 8


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

-- instance HasTime (Timed Int ())

beatSession :: (MonadIO m) =>
               Session m (Timed Int ())
beatSession =
  Session $ do
--    p0 <- liftIO $ atomically $ waitN pos 1
    return (Timed 0 (), loop)
  where
    loop =
      Session $ do
--        dp <- liftIO $ atomically $ waitN' pos 1
--        liftIO $ print dp
--        let dp = p - p'
        return (Timed 1 (), loop)

runShow :: (MonadIO m) =>
           TMVar MIDI
        -> SerialPort
        -> Session m (Timed Int ())
        -> MidiLights
        -> m a
runShow midi serialPort s0 w0 = loop s0 w0
  where
    loop s' w' = do
      m <- liftIO $ atomically $ takeTMVar midi
      (ds, s) <-
        case m of
          MidiClock -> stepSession s'
          _         -> do -- liftIO $ print m
                          return (Timed 0 (), s')

--      (ds, s) <- stepSession s'
--      mm      <- liftIO $ atomically $ tryTakeTMVar midi
      let Identity (mx, w) = stepWire w' ds (Right $ Event m) {- $ case mm of
                                                       Nothing -> NoEvent
                                                       (Just m) -> Event m
                                            ) -}
      case mx of
        (Right (Event actions)) ->
          do let perform (Print s)     = liftIO $ putStrLn s
                 perform (SendCmd cmd) = liftIO $ send serialPort (singleton $ serializeCommand cmd) >> return ()
             mapM_ perform actions
        _                 -> return ()
      loop s w

midiConsumer :: TMVar MIDI
             -> Consumer MIDI IO ()
midiConsumer t =
   forever $ do
     m <- await
     lift $ atomically $ putTMVar t m

blinkomatic :: String -> String -> MidiLights -> IO ()
blinkomatic midiDevice1 midiDevice2 ml =
  do t   <- atomically newEmptyTMVar
     mh1 <- openInput midiDevice1 None
     mh2 <- openInput midiDevice2 None
     case (mh1, mh2) of
       (Left e, Left e') ->
         do putStrLn =<< strError e
            exitFailure
       (Right h1, Right h2) ->
         do putStrLn "opened MPD32 MIDI Input."
            forkIO $ runEffect $ (lift $ RawMidi.read h2) >~ parseMidi >-> midiConsumer t
            forkIO $ runEffect $ (lift $ RawMidi.read h1) >~ parseMidi >-> midiConsumer t
            s <- openSerial port defaultSerialSettings { commSpeed = baud }
            runShow t s beatSession ml


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

sixteenth, eighth, quarter, whole :: Int
sixteenth  = 6
eighth     = 12
quarter    = 24
half       = 48
whole      = 96

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


oneMeasure :: MidiLights
oneMeasure =
  periodic quarter . loop
  where
    loop =
      for half . (pure [ SendCmd $ Command red        Close
                       , SendCmd $ Command darkOrange Open]) -->
      for half . (pure [ SendCmd $ Command darkOrange Close
                       , SendCmd $ Command red        Open]) -->
      loop

-- instance HasTime Int (Timed Int ())

nowNow :: MidiLights
nowNow =
  periodic 1 . (pure [Print "now"])
{-
asum :: Alternative f => [f a] -> f a
asum l =
  foldr (<|>) empty l
-}
oneMeasure2 :: MidiLights
oneMeasure2 =
  let redOnly        = [ SendCmd $ Command red        Close
                       , SendCmd $ Command darkOrange Open
                       , SendCmd $ Command yellow     Open
                       ]
      darkOrangeOnly = [ SendCmd $ Command darkOrange Close
                       , SendCmd $ Command red        Open
                       , SendCmd $ Command yellow     Open
                       ]
  in
   periodic 1 .
    proc midi ->
      do t <- fmap (`mod` whole) time -< ()
         asum [ when (== 0)    >>> pure redOnly
              , when (== half) >>> pure darkOrangeOnly
              ] -< t




strobe :: MidiLights
strobe =
  let onTime = 1 in
  periodic 1 .
    proc _ ->
     do t <- fmap (`mod` quarter) time -< ()
        asum [ when (== 0)          >>> pure [SendCmd $ Command whiteStrip         Close]
             , when (== 0 + onTime) >>> pure [SendCmd $ Command whiteStrip         Open]
             ] -< t

newChase :: MidiLights
newChase =
  periodic 1 .
   proc _ ->
    do t <- fmap (`mod` (whole * 2)) time -< ()
       asum [ when (== quarter * 0) >>> pure [SendCmd $ Command red         Close, SendCmd $ Command white       Open]
            , when (== quarter * 1) >>> pure [SendCmd $ Command darkOrange  Close, SendCmd $ Command red         Open]
            , when (== quarter * 2) >>> pure [SendCmd $ Command lightOrange Close, SendCmd $ Command darkOrange  Open]
            , when (== quarter * 3) >>> pure [SendCmd $ Command yellow      Close, SendCmd $ Command lightOrange Open]
            , when (== quarter * 4) >>> pure [SendCmd $ Command green       Close, SendCmd $ Command yellow      Open]
            , when (== quarter * 5) >>> pure [SendCmd $ Command blue        Close, SendCmd $ Command green       Open]
            , when (== quarter * 6) >>> pure [SendCmd $ Command violet      Close, SendCmd $ Command blue        Open]
            , when (== quarter * 7) >>> pure [SendCmd $ Command white       Close, SendCmd $ Command violet      Open]
            ] -< t


sequences :: MidiWire (Event MIDI, Event Integer) (Event [OutputEvent])
sequences =
  modes 0 pickMode
  where
    pickMode 0 = newChase
    pickMode 1 = strobe

modeWatcher' :: MidiWire (Event MIDI) (Event [OutputEvent])
modeWatcher' =
  proc e ->
    case e of
      (Event MidiClock) -> returnA -< NoEvent
      (Event m) -> returnA -< Event [Print $ show m]
      NoEvent   -> returnA -< NoEvent
modeWatcher =  modeWatcher' -- --> modeWatcher'

modeSwitcher :: MidiWire (Event MIDI) (Event MIDI, Event Integer)
modeSwitcher =
  proc e ->
    case e of
--      (Event MidiClock) -> returnA -< NoEvent
      (Event m@(NoteOn _channel (NoteNumber nn) _velocity)) ->
        case nn of
          36 -> returnA -< (Event m, Event 0)
          37 -> returnA -< (Event m, Event 1)
          _  -> returnA -< (Event m, NoEvent)
      (Event m) ->
        returnA -< (Event m, NoEvent)
      NoEvent   -> returnA -< (NoEvent, NoEvent)


  {-
  where
    toOutput :: MIDI -> [OutputEvent]
    toOutput = undefined
-}
--    toOutput (Event midi) = []
--      case midi of
--        _ -> Event []
--        MidiClock -> []
--        n -> Event [Print $ show n]
  {-
  proc midi ->
    case midi of
      (Event MidiClock) -> returnA -< []
      (Event n) -> returnA -< [Print $ show n]
      NoEvent   -> returnA -< []
-}

--      _ -> returnA -< NoEvent


         {-
         msum [ (pure redOnly . when (== 0)) -< t
              , (pure darkOrangeOnly . when (== half)) -< t
              ]
-}

--       returnA -< e
--       (fmap (const []) $ became (>= 0)) -< t

--         . pure [SendCmd $ Command red        Close, SendCmd $ Command darkOrange Open, SendCmd $ Command yellow Open]
--       case t `mod` whole of
--         0                 -> now -<  [SendCmd $ Command red        Close, SendCmd $ Command darkOrange Open, SendCmd $ Command yellow Open]
--         t' | t' == eighth -> now -<  [SendCmd $ Command darkOrange Close, SendCmd $ Command red        Open, SendCmd $ Command yellow Open]
--         _ -> returnA -< [Print $ show t]
  
  {-
oneMeasure2 :: MidiLights
oneMeasure2 = periodic 1 .
  proc midi ->
    do t <- time -< ()
       case t `mod` whole of
--         0                 -> now -<  [SendCmd $ Command red        Close, SendCmd $ Command darkOrange Open, SendCmd $ Command yellow Open]
--         t' | t' == eighth -> now -<  [SendCmd $ Command darkOrange Close, SendCmd $ Command red        Open, SendCmd $ Command yellow Open]
         _ -> now -< [Print $ show t]
-}
--         _                 -> now -<  [SendCmd $ Command darkOrange Open, SendCmd $ Command red        Open, SendCmd $ Command yellow Close]

--  ) --> oneMeasure2 -- SendCmd $ Command darkOrange Open, SendCmd $ Command red        Open, SendCmd $ Command yellow Close]
--              returnA -< e -- . delay [SendCmd $ Command red         Close, SendCmd $ Command white       Open]

--          half -> pure [SendCmd $ Command darkOrange  Close, SendCmd $ Command red         Open]

--  fmap (const $ Event []) $ became (\t -> t `mod` whole == 0) . time
--    ((for half . pure [SendCmd $ Command red         Close, SendCmd $ Command white       Open]) <>
--      (after half . pure [SendCmd $ Command darkOrange  Close, SendCmd $ Command red         Open])
--    )
     {-
    (for period . (now . pure [SendCmd $ Command lightOrange Close, SendCmd $ Command darkOrange  Open]) -->
    (for period . (now . pure [SendCmd $ Command yellow      Close, SendCmd $ Command lightOrange Open]) -->
    (for period . (now . pure [SendCmd $ Command green       Close, SendCmd $ Command yellow      Open]) -->
    (for period . (now . pure [SendCmd $ Command blue        Close, SendCmd $ Command green       Open]) -->
    (for period . (now . pure [SendCmd $ Command violet      Close, SendCmd $ Command blue        Open]) -->
    (for period . (now . pure [SendCmd $ Command white       Close, SendCmd $ Command violet      Open]) -->
-}


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
