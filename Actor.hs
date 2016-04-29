{-# LANGUAGE OverloadedStrings #-}
module Actor (
  unit, Actor, Data((:@)), Message, Address, Handler(..),
  spawn, send, runActor, debug) where

import Data.Word
import System.Random
import qualified Crypto.Hash.SHA1 as SHA1
import Data.String
import Control.Monad.Trans
import Control.Monad
import Control.Concurrent
import System.IO.Unsafe
import Text.Printf
import qualified Data.ByteString as BS

data Data =
  DNull |
  DAddr Integer (Chan (MVar Data, Message)) |
  DInt Integer |
  DStr String |
  Data :@ Data |
  DFut (MVar Data)

type Message = Data
type Address = Data

unit = DNull
instance Num Data where
  fromInteger i = DInt i
  DInt x + DInt y = DInt (x + y)
  _ + _ = error "incompatible dynamic types for Data +"
  DInt x * DInt y = DInt (x * y)
  _ * _ = error "incompatible dynamic types for Data *"
  negate (DInt x) = DInt (negate x)
  negate _ = error "Data negate: not a number"
  abs (DInt x) = DInt (abs x)
  abs _ = error "Data abs: not a number"
  signum (DInt x) = DInt (signum x)
  signum _ = error "Data signum: not a number"

instance Monoid Data where
  mempty = DStr ""
  mappend (DStr x) (DStr y) = DStr (x ++ y)
  mappend _ _ = error "Data <>: incompatible dynamic types for concat"

instance Integral Data where
  toInteger (DInt i) = i
  quotRem (DInt i) (DInt j) = let (x,y) = quotRem i j in (DInt x, DInt y)

instance Real Data where
  toRational (DInt i) = fromIntegral i

instance Enum Data where
  toEnum i = DInt (fromIntegral i)
  fromEnum (DInt i) = fromIntegral i

instance Eq Data where
  DNull == DNull         = True
  DAddr i _ == DAddr j _ = i == j
  DInt i == DInt j       = i == j
  DStr s == DStr u       = s == u
  x :@ y == z :@ w       = x == z && y == w
  d@(DFut _) == y        = force d == force y
  x == d@(DFut _)        = force x == force d
  _ == _                 = False

instance Ord Data where
  compare DNull DNull = EQ
  compare (DAddr i _) (DAddr j _) = compare i j
  compare (DInt i) (DInt j) = compare i j
  compare (DStr s) (DStr u) = compare s u
  compare (x :@ y) (z :@ w) = compare x z `mappend` compare y w
  compare d@(DFut _) y      = compare (force d) (force y)
  compare x d@(DFut _)      = compare (force x) (force d)
  compare x y = compare (level x) (level y)

level :: Data -> Int
level DNull = 0
level (DAddr _ _) = 1
level (DInt _) = 2
level (DStr _) = 3
level (_ :@ _) = 4
level d@(DFut _) = level (force d)

instance IsString Data where
  fromString s = DStr s

instance Show Data where
  show DNull = "()"
  show (DAddr n _) =
    let bytes = (BS.take 8 . SHA1.hash . BS.pack . smash) n in
    let hex = concatMap (printf "%02x") (BS.unpack bytes) in
    "AddressFingerprint " ++ hex
  show (DInt n) = show n
  show (DStr s) = show s
  show (x :@ y) =
    let d0 = depth x in
    let d1 = depth y in
    (if depth x > 0 then "(" ++ show x ++ ")" else show x) ++ " :@ " ++
    (if depth y > 0 then "(" ++ show y ++ ")" else show y)
  show d@(DFut _) = show (force d)

depth :: Data -> Int
depth d = f 0 d where
  f i (x :@ y) = max (f (i+1) x) (f (i+1) y)
  f i d@(DFut _) = f i (force d)
  f i _ = i

force :: Data -> Data
force (DFut mv) = force (unsafePerformIO (readMVar mv))
force d = d

smash :: Integer -> [Word8]
smash 0 = []
smash i = let (q,r) = divMod i 256 in fromIntegral r : smash q

newtype Handler = Handler { runHandler :: Message -> Actor (Data, Handler) }
data Actor a = Actor (MVar Integer -> IO a)

instance Monad Actor where
  return x = Actor (\_ -> return x)
  Actor run >>= f = Actor $ \counter -> do
    x <- run counter
    let Actor run' = f x
    run' counter

instance Applicative Actor where
  pure = return
  af <*> ax = do
    f <- af
    x <- ax
    return (f x)

instance Functor Actor where
  fmap f (Actor run) = Actor (\counter -> fmap f (run counter))

instance MonadIO Actor where
  liftIO io = Actor $ \_ -> io

runActor :: (Address -> Handler) -> IO a
runActor mkH = do
  let Actor run = spawn mkH
  i <- randomRIO (2^128, 2^129)
  counter <- newMVar i
  DAddr _ ch <- run counter
  mv' <- newEmptyMVar
  writeChan ch (mv', "start")
  answer <- takeMVar mv'
  print answer
  forever (threadDelay 10000000)

spawn :: (Address -> Handler) -> Actor Address
spawn mkH = Actor $ \counter -> do
  ch <- newChan
  i <- modifyMVar counter (\i -> return (i+1, i))
  let addr = DAddr i ch
  forkIO (actor counter ch (mkH addr))
  return addr

send :: Address -> Message -> Actor Data
send addr m = Actor $ \_ -> do
  let (DAddr _ ch) = addr
  mv' <- newEmptyMVar
  writeChan ch (mv', m)
  return (DFut mv')

actor :: MVar Integer -> Chan (MVar Data, Message) -> Handler -> IO ()
actor counter ch (Handler h) = do
  (mv', m) <- readChan ch
  let (Actor run) = h m
  (x, h') <- run counter
  putMVar mv' x
  actor counter ch h'

debug :: Show a => a -> Actor ()
debug x = do
  liftIO (putStr "DEBUG: ")
  liftIO (putStrLn (show x))
