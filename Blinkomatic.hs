{-# LANGUAGE Arrows, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
module Blinkomatic where

import Control.Arrow
-- import Control.Applicative
import Control.Concurrent           (ThreadId, forkIO, threadDelay)
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
import Color
import Data.Bits           ((.|.), shiftL)
import Data.ByteString     (ByteString, pack, singleton)
import qualified Data.ByteString as B
import Data.Data (Data, Typeable)
import Data.Foldable       (asum, foldMap)
import Data.Time.Clock     (addUTCTime, diffUTCTime, getCurrentTime)
import Data.Monoid         (Monoid(mappend, mempty))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import Debug.Trace (trace)
import FRP.Netwire.Move (integral)
import Pipes          ((>~), (>->), Consumer, await, runEffect)
import Prelude hiding ((.), id, until)
import Sound.NH.MIDI.Core
import Sound.NH.MIDI.Parse   (parseMidi)
import Sound.NH.ALSA.RawMidi as RawMidi (RawMode(None), openInput, read, strError)
import System.Hardware.Serialport (CommSpeed(CS9600, CS115200), SerialPort, SerialPortSettings(commSpeed), defaultSerialSettings, openSerial, send)
import System.Exit (exitFailure)

instance (Semigroup a) => Semigroup (Vector a) where
  a <> b =  a `mappend` b

scaleRGB :: (Num a) => RGB a -> a -> RGB a
scaleRGB (RGB r g b) s = RGB (r*s) (g*s) (b*s)

packRGB :: RGB Word8
        -> ByteString
packRGB (RGB r g b) = pack [r,g,b]

packRGBs :: Vector (RGB Word8)
         -> ByteString
packRGBs rgbs = foldMap packRGB rgbs

-- | FIXME: handle longer than 254
-- FIXME: handle 0xFF and non-zero termination
stuff :: ByteString -> ByteString
stuff b | B.length b > 254 = error "stuff not implemented for ByteStrings longer than 254 bytes. Submit a patch!"
stuff b = (B.cons 0 (stuff' (B.snoc b 0)))
  where
    stuff' :: ByteString -> ByteString
    stuff' b | B.null b = b
    stuff' b =
      case B.span (/= 0) b of
        (b0, b1) ->
          (B.cons (fromIntegral $ B.length b0 + 1) b0) <> (stuff' $ B.drop 1 b1)

elPort = "/dev/ttyUSB0"
elBaud = CS9600

tclPort = "/dev/ttyACM0"
tclBaud = CS115200

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
  | TCL (Vector (RGB Word8))
  | Print String
    deriving Show

instance (Show a) => Show (Event a) where
  show (Event a) = "Event " ++ show a
  show NoEvent   = "NoEvent"

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

type MidiTimed   = Timed Int ()
type MidiSession m = Session m MidiTimed
type MidiWire    = Wire MidiTimed () Identity
type MidiLights  = Wire MidiTimed () Identity (Event MIDI) (Event [OutputEvent])

-- instance HasTime (Timed Int ())

beatSession :: (MonadIO m) => MidiSession m
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

clockThread :: TMVar MIDI -> Double -> IO ThreadId
clockThread tm bpm = forkIO $
  do t0 <- getCurrentTime
     loop t0
  where
    secondsPerPulse = ((1 / (realToFrac bpm)) * 60) / 24
    loop t' =
      do t <- getCurrentTime
         let dt = diffUTCTime t t'
         if dt > secondsPerPulse
           then do atomically $ putTMVar tm MidiClock
--                   putStrLn "MidiClock sent."
--                   threadDelay 10
                   loop (addUTCTime secondsPerPulse t')
           else loop t'

runShow :: (MonadIO m) =>
           TMVar MIDI
        -> (OutputEvent -> m ())
        -> MidiSession m
        -> MidiLights
        -> m a
runShow midi output s0 w0 = loop s0 w0
  where
    loop s' w' = do
      m <- liftIO $ atomically $ takeTMVar midi
--      liftIO $ print m
      (ds, s) <-
        case m of
          MidiClock -> stepSession s'
          _         -> do -- liftIO $ print m
                          return (Timed 0 (), s')
      let Identity (mx, w) = stepWire w' ds (Right $ Event m)
      case mx of
        (Right (Event actions)) ->
          do mapM_ output actions
        _                 -> return ()
      loop s w

midiConsumer :: TMVar MIDI
             -> Consumer MIDI IO ()
midiConsumer t =
   forever $ do
     m <- await
     lift $ atomically $ putTMVar t m

standardOutput :: (MonadIO m) =>
                  Maybe SerialPort
               -> Maybe SerialPort
               -> OutputEvent
               -> m ()
standardOutput _ _ (Print str)   = liftIO $ putStrLn str
standardOutput (Just s) _ (SendCmd cmd) = liftIO $ send s (singleton $ serializeCommand cmd) >> return ()
standardOutput Nothing  _ (SendCmd _) = return ()
standardOutput _ (Just s) (TCL v)       = liftIO $ send s (stuff $ packRGBs v) >> return ()

blinkomatic :: TMVar MIDI -> MidiLights -> IO ()
blinkomatic t ml =
  do -- s1 <- openSerial port defaultSerialSettings { commSpeed = baud }
     s2 <- openSerial tclPort defaultSerialSettings { commSpeed = tclBaud }
     runShow t (standardOutput Nothing (Just s2)) beatSession ml

openMidi :: TMVar MIDI -> [String] -> IO ()
openMidi t names =
  do handles <- mapM (\name -> openInput name None) names
     mapM_ (\(Right h) -> forkIO $ runEffect $ (lift $ RawMidi.read h) >~ parseMidi >-> midiConsumer t) handles

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

tclRedBlue :: MidiLights
tclRedBlue =
  for quarter . now . pure [TCL $ V.replicate 25 (RGB 0xff 0x00 0x00)] -->
  for quarter . now . pure [TCL $ V.replicate 25 (RGB 0x00 0x00 0xff)] -->
  tclRedBlue

tclFade :: MidiLights
tclFade =
  let fadeIn =
        proc midi ->
          do t <- time -< ()
             v <- integral 0 -< (fromIntegral t)
             q <- (periodic 1 . arr (\v -> [TCL $ V.replicate 25 $ RGB (floor v) 0x00 0x00])) &&& (became (> 255)) -< v
             until -< q
      fadeOut =
        proc midi ->
          do t <- time -< ()
             v <- integral 0 -< (fromIntegral t)
             q <- (periodic 1 . arr (\v -> [TCL $ V.replicate 25 $ RGB (0xff - (floor v)) 0x00 0x00])) &&& (became (> 255)) -< v
             until -< q
  in fadeIn --> fadeOut --> tclFade

tclSlider :: MidiLights
tclSlider =
  let right =
        proc midi ->
          do t <- time -< ()
             v <- id -< (fromIntegral t)
             q <- (periodic 1 . arr (\v -> [TCL $ V.generate 25 (\i -> if i <= (floor v) then (RGB 0xff 0x00 0x00) else (RGB 0x00 0x00 0x00))])) &&& (became (> 24)) -< v
             until -< q
      left =
        proc midi ->
          do t <- time -< ()
             v <- id -< (fromIntegral t)
             q <- (periodic 1 . arr (\v -> [TCL $ V.generate 25 (\i -> if i >= (floor v) then (RGB 0xff 0x00 0x00) else (RGB 0x00 0x00 0x00))])) &&& (became (> 24)) -< (v :: Double)
             until -< q
  in right --> left --> tclSlider


tclSlider2 :: MidiLights
tclSlider2 =
  let right =
        proc midi ->
          do t <- time -< ()
             v <- arr (/ 4) -< (fromIntegral t)
             q <- (periodic 1 . arr (\v -> [TCL $ V.generate 25 (\i -> if i <= (floor v) then (RGB (floor $ 255 * (fromIntegral i)/v) 0x00 0x00) else (RGB 0x00 0x00 0x00))])) &&& (became (> 24)) -< (v :: Double)
             until -< q
  in right --> tclSlider2


tclSlider3 :: MidiLights
tclSlider3 =
  let slideR, slideL :: RGB Word8 -> MidiWire Int (Vector (RGB Word8))
      slideR color =
        proc t ->
          do v <- arr (\t' -> V.generate 25 (\i -> if i <= t' then color else blackRGB)) -< t
             b <- became (> 24) -< t
             until -< (v, b)

      -- similar to slideR but more pointless
      slideL color =
           until . (arr (\t' -> V.generate 25 (\i -> if i >= (24 - t') then color else blackRGB)) &&& (became (> 24)))

      loop =
          (proc _ ->
            do t <- fmap (`div` 4) time  -< ()
               l <- slideL redRGB  -< t
               r <- slideR blueRGB -< t
               returnA -< [TCL $ V.zipWith (<>) l r]
          ) --> loop
  in periodic 1 . loop

rainbow :: MidiLights
rainbow =
  periodic 1 .
    (proc _ ->
        do t <- time -< ()
           returnA -< [TCL $ V.generate 25 $ \i -> rgb_d2w $ hsl2rgb $ HSL ((360/25)*(fromIntegral ((i + (t `div` 4)) `mod` 25))) 1 0.5])

rainbow2 :: MidiLights
rainbow2 =
  periodic 1 .
    (proc _ ->
        do t <- time -< ()
           returnA -< [TCL $ V.generate 25 $ \i -> rgb_d2w $ hsl2rgb $ HSL (h i ((t * 4) `mod` 360)) 1 0.5])
  where
    h i t =
      case ((360/25)*(fromIntegral i)+(fromIntegral t)) of
        h' | h' >= 360 -> h' - 360
           | otherwise -> h'

decayElem :: MidiWire () (RGB Word8)
decayElem =
  proc e ->
    do t <- (arr $ (* 2)) . time -< e
       until . (arr (\t -> RGB (255 - (fromIntegral t)) 0 0) &&& (became (>= 255))) -< t

decay :: MidiLights
decay = (for 300 . never) -->
  proc _ ->
   do e <- decayElem -< ()
      periodic 1 -< [TCL $ V.replicate 25 e]
--       1 -< [TCL $ V.replicate 25 e]

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

nowNow :: MidiLights
nowNow =
  periodic 1 . (pure [Print "now"])

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

showMidi :: MidiLights
showMidi =
  accumE (\_ m -> [Print $ show m]) []


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
