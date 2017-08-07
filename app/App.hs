module App 
  ( Key
  , ProgressionStep (..)
  , State (..)
  , SharedScaleNote (..)
  , progressionStep
  , findSharedNotes
  , markScaleSpan
  ) where

import Data.List (nub)
import Music (Note (..), Chord (..), Scale (..), inScale, scalesForChord, chordsForScale)

type Key = (Note, Bool)

data ProgressionStep = Step { scales :: [Scale]
                            , chords :: [Chord]
                            , editingScale :: Bool
                            , editingChord :: Bool
                            } deriving Eq

progressionStep :: Chord -> Bool -> ProgressionStep
progressionStep c e = Step { scales = scalesForChord c
                           , chords = nub $ c : (chordsForScale $ head $ scalesForChord c)
                           , editingScale = e
                           , editingChord = False }

data State = State { quitting :: Bool
                   , progression :: [ProgressionStep]
                   , keys :: [Key]
                   } deriving Eq

data SharedScaleNote = SSN { note :: Note 
                           , inPrev :: Bool
                           , inThis :: Bool
                           , inNext :: Bool
                           }

-- partOfScale :: [Note] -> Scale -> [(Note, Bool)]
-- partOfScale ns s = map (\n -> (n, inScale s n)) ns

findSharedNotes :: [Note] -> Maybe Scale -> Scale -> Maybe Scale -> [SharedScaleNote]
findSharedNotes ns prev this next = map (\n -> SSN { note = n
                                                   , inPrev = maybe False (flip inScale n) prev
                                                   , inThis = inScale this n
                                                   , inNext = maybe False (flip inScale n) next
                                                   }) ns

markScaleSpan :: Scale -> [Note] -> [Bool]
markScaleSpan (Scale root _) = reverse . foldl markNote [] where
  markNote :: [Bool] -> Note -> [Bool]
  markNote [] n = [n == root]
  markNote (marked:ms) n | n == root = (not marked):marked:ms
                         | otherwise = marked:marked:ms