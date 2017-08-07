module Music
  ( Note(..), Chord (..), ChordType (..), Scale (..), ScaleType (..)
  , notes
  , scales
  , chordTypes
  , scaleNotes
  , chordNotes
  , chordsForNotes
  , scalesForChord
  , scalesForProgression
  , inScale
  , chordsForScale
  ) where

import Data.List

data Note = A | Bb | B | C | Cs | D | Eb | E | F | Fs | G | Ab
  deriving (Eq, Ord)

instance Show Note where
  show A = "A"
  show Bb = "Bb"
  show B = "B"
  show C = "C"
  show Cs = "C#"
  show D = "D"
  show Eb = "Eb"
  show E = "E"
  show F = "F"
  show Fs = "F#"
  show G = "G"
  show Ab = "Ab"

notes = [C, Cs, D, Eb, E, F, Fs, G, Ab, A, Bb, B]

fromNote :: Note -> Int
fromNote = maybe 0 id . flip elemIndex notes

toNote :: Int -> Note
toNote n = notes !! (n `mod` 12)

-- data ScalePosition = I | II | III | IV | V | VI | VII | VIII
--   deriving (Show, Eq, Ord)

-- scalePositions = [I, II, III, IV, V, VI, VII, VIII]

-- fromScalePosition :: ScalePosition -> Int
-- fromScalePosition = maybe 1 id . flip elemIndex scalePositions

-- toScalePosition :: Int -> ScalePosition
-- toScalePosition n = scalePositions !! (n `mod` 7)

data ScaleType = Major | Minor | Diminished | WholeTone
  deriving (Show, Eq, Ord)

-- scaleTypes = [Major, Minor, Diminished, WholeTone]

flatten :: Note -> Note
flatten = toNote . subtract 1 . fromNote

sharpen :: Note -> Note
sharpen = toNote . (+ 1) . fromNote

scaleIntervals :: ScaleType -> [Int]
scaleIntervals Major =      [2, 2, 1, 2, 2, 2]
scaleIntervals Minor =      [2, 1, 2, 2, 2, 2]
scaleIntervals Diminished = [1, 2, 1, 2, 1, 2, 1]
scaleIntervals WholeTone =  [2, 2, 2, 2, 2]

data Scale = Scale Note ScaleType
  deriving (Eq, Ord)

instance Show Scale where
  show scale = case scale of
      (Scale root Diminished) -> show' Diminished $ expandRoots 3 root
      (Scale root WholeTone)  -> show' WholeTone $ expandRoots 2 root
      (Scale root stype)      -> show' stype [root]
    where 
      show' :: ScaleType -> [Note] -> String
      show' stype roots = unwords $ [intercalate "/" . map show $ roots, show stype]

      expandRoots :: Int -> Note -> [Note]
      expandRoots step = map toNote . take (12 `div` step) . iterate (+ step) . fromNote

scales = [Scale root stype | stype <- [Major, Minor], root <- notes]
      ++ [Scale A Diminished, Scale Bb Diminished, Scale B Diminished]
      ++ [Scale A WholeTone, Scale Bb WholeTone]

scaleNotes' :: ScaleType -> Note -> [Note]
scaleNotes' stype root = map toNote $ scanl (+) (fromNote root) (scaleIntervals stype)

scaleNotes :: Scale -> [Note]
scaleNotes (Scale root stype) = scaleNotes' stype root

scaleNote :: Int -> Note -> Note
scaleNote p root = let ns = scaleNotes' Major root in ns !! (p `mod` length ns)

inScale :: Scale -> Note -> Bool
inScale s n = n `elem` (scaleNotes s)

i = scaleNote 0
-- ii = scaleNote 1
iii = scaleNote 2
-- iv = scaleNote 3
v = scaleNote 4
vi = scaleNote 5
vii = scaleNote 6
ix = scaleNote 8
xi = scaleNote 10
xiii = scaleNote 12

fl :: (Note -> Note) -> (Note -> Note)
fl = (flatten .)

sh :: (Note -> Note) -> (Note -> Note)
sh = (sharpen .)

data ChordType = Maj | Maj7
               | Dom7 | Dom7b5 | Dom7sh11
               | Min | Min7 | MinMaj7 | Min7b5
               | Alt
               | Dim deriving Eq

instance Show ChordType where
  show Maj = ""
  show Maj7 = "Maj7"
  show Dom7 = "7"
  show Dom7b5 = "7b5"
  show Dom7sh11 = "7#11"
  show Min = "m"
  show Min7 = "m7"
  show MinMaj7 = "minMaj7"
  show Alt = "alt"
  show Dim = "o"
  show Min7b5 = "m7b5"

chordScalePositions :: ChordType -> [Note -> Note]
chordScalePositions Maj = [i, iii, v]
chordScalePositions Maj7 = [i, iii, vii]
chordScalePositions Dom7 = [i, iii, fl vii]
chordScalePositions Dom7b5 = [i, iii, fl v, fl vii]
chordScalePositions Dom7sh11 = [i, iii, fl v, sh xi]
chordScalePositions Min = [i, fl iii, v]
chordScalePositions Min7 = [i, fl iii, v, fl vii]
chordScalePositions MinMaj7 = [i, fl iii, v, vii]
chordScalePositions Min7b5 = [i, fl iii, fl v, fl vii]
chordScalePositions Alt = [i, fl iii, fl v, fl vii, fl ix, fl xi, fl xiii]
chordScalePositions Dim = [i, fl iii, fl v, vi]

chordTypes = [Maj, Min, Maj7, Dom7, Min7, MinMaj7, Alt, Dim, Min7b5, Dom7sh11, Dom7b5]

data Chord = Chord Note ChordType deriving Eq

instance Show Chord where
  show (Chord n ct) = show n ++ show ct

chordNotes' :: ChordType -> Note -> [Note]
chordNotes' ct root = map ($ root) $ chordScalePositions ct

chordNotes :: Chord -> [Note]
chordNotes (Chord root ct) = chordNotes' ct root

chordsForNotes :: [Note] -> [Chord]
chordsForNotes ns = [Chord root ct | ct <- chordTypes, root <- notes, ns `containedIn` (chordNotes' ct root)]
  where containedIn as bs = sort (as `intersect` bs) == sort as

scalesForChord :: Chord -> [Scale]
scalesForChord c = [s | s <- scales, (chordNotes c) `containedIn` (scaleNotes s)] 
  where containedIn as bs = sort (as `intersect` bs) == sort as

chordsForScale :: Scale -> [Chord]
chordsForScale s = [Chord root ct | ct <- chordTypes, root <- notes, (chordNotes' ct root) `containedIn` (scaleNotes s)]
  where containedIn as bs = sort (as `intersect` bs) == sort as

commonNotes :: Scale -> Scale -> [Note]
commonNotes a b = sort $ scaleNotes a `intersect` scaleNotes b

scalesForProgression :: Chord -> Chord -> [(Scale, Scale, [Note])]
scalesForProgression a b = reverse $ sortOn (\(_, _, n) -> length n) [(sa, sb, commonNotes sa sb) | sa <- scalesForChord a, sb <- scalesForChord b]

