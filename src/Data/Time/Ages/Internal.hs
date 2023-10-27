{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Time.Ages.Internal where

import qualified Control.Concurrent as Conc
import qualified System.Timeout as Conc

newtype Duration = Duration {getPicos :: Integer}
  deriving (Show, Read, Eq, Ord)

-- | Use instance to sum durations
instance Semigroup Duration where
  Duration a <> Duration b = Duration (a + b)

-- | mempty == PPicoseconds 0
instance Monoid Duration where
  mempty = Duration 0

data Unit
  = Picoseconds
  | Nanoseconds
  | Microseconds
  | Milliseconds
  | Seconds
  | Minutes
  | Hours
  | Days
  | Weeks
  deriving (Enum, Bounded, Eq, Ord, Show, Read)

-- | Bidirectional pattern for picoseconds. Matches ONLY INTEGER part of value
{-# COMPLETE PPicoseconds #-}
pattern PPicoseconds :: Integer -> Duration
pattern PPicoseconds n = Duration n

-- | Bidirectional pattern for nanoseconds. Matches ONLY INTEGER part of value
{-# COMPLETE PNanoseconds #-}
pattern PNanoseconds :: Integer -> Duration
pattern PNanoseconds n <- (divDuration (factor Nanoseconds) -> Duration n)
  where
    PNanoseconds n = Duration (n * factor Nanoseconds)

-- | Bidirectional pattern for microseconds. Matches ONLY INTEGER part of value
{-# COMPLETE PMicroseconds #-}
pattern PMicroseconds :: Integer -> Duration
pattern PMicroseconds n <- (divDuration (factor Microseconds) -> Duration n)
  where
    PMicroseconds n = Duration (n * factor Microseconds)

-- | Bidirectional pattern for milliseconds. Matches ONLY INTEGER part of value
{-# COMPLETE PMilliseconds #-}
pattern PMilliseconds :: Integer -> Duration
pattern PMilliseconds n <- (divDuration (factor Milliseconds) -> Duration n)
  where
    PMilliseconds n = Duration (n * factor Milliseconds)

-- | Bidirectional pattern for seconds. Matches ONLY INTEGER part of value
{-# COMPLETE PSeconds #-}
pattern PSeconds :: Integer -> Duration
pattern PSeconds n <- (divDuration (factor Seconds) -> Duration n)
  where
    PSeconds n = Duration (n * factor Seconds)

-- | Bidirectional pattern for minutes. Matches ONLY INTEGER part of value
{-# COMPLETE PMinutes #-}
pattern PMinutes :: Integer -> Duration
pattern PMinutes n <- (divDuration (factor Minutes) -> Duration n)
  where
    PMinutes n = Duration (n * factor Minutes)

-- | Bidirectional pattern for hours. Matches ONLY INTEGER part of value
{-# COMPLETE PHours #-}
pattern PHours :: Integer -> Duration
pattern PHours n <- (divDuration (factor Hours) -> Duration n)
  where
    PHours n = Duration (n * factor Hours)

-- | Bidirectional pattern for days. Matches ONLY INTEGER part of value
{-# COMPLETE PDays #-}
pattern PDays :: Integer -> Duration
pattern PDays n <- (divDuration (factor Days) -> Duration n)
  where
    PDays n = Duration (n * factor Days)

-- | Bidirectional pattern for weeks. Matches ONLY INTEGER part of value
{-# COMPLETE PWeeks #-}
pattern PWeeks :: Integer -> Duration
pattern PWeeks n <- (divDuration (factor Days) -> Duration n)
  where
    PWeeks n = Duration (n * factor Weeks)



nanoFactor :: Integer
nanoFactor = 10e3

microFactor :: Integer
microFactor = 10e6

milliFactor :: Integer
milliFactor = 10e9

secondFactor :: Integer
secondFactor = 10e12

minuteFactor :: Integer
minuteFactor = secondFactor * 60

hourFactor :: Integer
hourFactor = minuteFactor * 60

dayFactor :: Integer
dayFactor = hourFactor * 24

weekFactor :: Integer
weekFactor = dayFactor * 7

factor :: (Num a) => Unit -> a
factor =
  fromInteger . \case
    Picoseconds -> 1
    Nanoseconds -> nanoFactor
    Microseconds -> microFactor
    Milliseconds -> milliFactor
    Seconds -> secondFactor
    Minutes -> minuteFactor
    Hours -> hourFactor
    Days -> dayFactor
    Weeks -> weekFactor

divDuration :: Integer -> Duration -> Duration
divDuration n (Duration x) = Duration (x `div` n)

mulDuration :: Integer -> Duration -> Duration
mulDuration n (Duration x) = Duration (x * n)

-- | Returns only integer part
toRawInt :: Duration -> Unit -> Integer
d `toRawInt` u = fromInteger case u of
  Picoseconds -> let PPicoseconds x = d in x
  Nanoseconds -> let PNanoseconds x = d in x
  Microseconds -> let PMicroseconds x = d in x
  Milliseconds -> let PMilliseconds x = d in x
  Seconds -> let PSeconds x = d in x
  Minutes -> let PMinutes x = d in x
  Hours -> let PHours x = d in x
  Days -> let PDays x = d in x
  Weeks -> let PWeeks x = d in x

toRaw :: Duration -> Unit -> Double
(Duration x) `toRaw` u = fromIntegral x / factor u

-- | Wrapped threadDelay
sleep :: Duration -> IO ()
sleep (PMicroseconds ms) = Conc.threadDelay (fromInteger ms)

-- | Wrapped timeout
timeout :: Duration -> IO a -> IO (Maybe a)
timeout (PMicroseconds ms) = Conc.timeout (fromInteger ms)
