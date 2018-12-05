module Lib where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8
import qualified Data.SortedList                  as SL

data Instr = Sleep | Wake | Start Int deriving (Eq, Show)

data Time = Time { year   :: !Int
                 , month  :: !Int
                 , day    :: !Int
                 , hour   :: !Int
                 , minute :: !Int } deriving (Eq)

instance Show Time where
  show time =
    show (year time) ++
    show (leftPad '0' 2 $ month time) ++
    show (leftPad '0' 2 $ day time) ++
    show (leftPad '0' 2 $ hour time) ++
    show (leftPad '0' 2 $ minute time)

instance Ord Time where
  x <= y = show x <= show y

data Record = Record { timestamp   :: Time
                     , instruction :: Instr } deriving (Eq, Show)

instance Ord Record where
  x <= y = timestamp x <= timestamp y

data Shift = Shift { start   :: Time
                   , sleep   :: Maybe Time
                   , wake    :: Maybe Time
                   , guardId :: Int }

isStart :: Record -> Bool
isStart record =
  case (instruction record) of
    (Start _) ->
      True

    _ ->
      False

beginShiftParse :: Parser Instr
beginShiftParse = do
  guard <- string "Guard #" *> decimal <* string " begins shift"
  return $ Start guard

leftPad :: Char -> Int -> Int -> String
leftPad filler size base =
  Prelude.take (size - length (show base)) (repeat filler) ++ (show base)

recordParser :: Parser Record
recordParser = do
  yr <- char '[' *> decimal <* char '-'
  mnth <- decimal <* char '-'
  dy <- decimal <* char ' '
  h <- decimal <* char ':'
  m <- decimal <* string "] "
  instr <- beginShiftParse <|>
    (const Sleep <$> string "falls asleep") <|>
    (const Wake <$> string "wakes up")
  return $ Record (Time yr mnth dy h m) instr


{- In a world where I do these puzzles not after APA nights, this finds the next components
of a shift and builds the Shift out of the components, but pool and beer and time and sleep
and I'm not going to finish this problem on the correct day I don't think
-}
recordsToGroups :: SL.SortedList Record -> [SL.SortedList Record]
recordsToGroups recs =
  case (SL.uncons recs) of
    Just (r, t) ->
      undefined
    Nothing ->
      []

recordsToShifts :: SL.SortedList Record -> [Shift]
recordsToShifts records =
  case (SL.uncons records) of
    Just (h, t) ->
      undefined

    Nothing ->
      []
