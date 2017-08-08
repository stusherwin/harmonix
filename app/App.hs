module App 
  ( Key (..)
  , ProgressionStep (..)
  , State (..)
  , Command (..)
  , Role (..)
  , ScaleRow (..)
  , ScaleRowNote (..)
  , progressionStep
  , handleCommand
  , key
  , scaleRows
  , buildKeys
  ) where

import Data.List (findIndex, nub)

import Music (Note (..), Chord (..), Scale (..), inScale, scalesForChord, chordsForScale)
import Util (rotate)

data Role = Prev | This | Next | None deriving Eq

data Key = Key { keyNote :: Note
               , pressed :: Bool
               , sharing :: (Bool, Bool, Bool)
               } deriving (Eq, Show)

key :: Note -> Bool -> Key
key n pr = Key { keyNote = n, pressed = pr, sharing = (False, False, False) }

data ScaleRowNote = ScaleRowNote { note :: Note
                                 , marked :: Bool
                                 , sharing' :: (Bool, Bool, Bool)
                                 } deriving (Eq, Show)

data ScaleRow = ScaleRow { scale :: Scale
                         , notes :: [ScaleRowNote]
                         } deriving (Eq, Show)

scaleRows :: [Note] -> [Scale] -> [ScaleRow]
scaleRows ns scs = scaleRows' (last scs:scs) (length scs) where
  scaleRows' :: [Scale] -> Int -> [ScaleRow]
  scaleRows' [] _       = error "Not enough scales"
  scaleRows' (_:[]) _   = error "Not enough scales"
  scaleRows' (_:_:[]) _ = error "Not enough scales"
  scaleRows' _ 0        = []
  scaleRows' ss@(s1:s2:s3:_) n = scaleRow (s1, s2, s3) : scaleRows' (rotate 1 ss) (n - 1)

  scaleRow :: (Scale, Scale, Scale) -> ScaleRow
  scaleRow (prev, this@(Scale root _), next) = ScaleRow { scale = this, notes = reverse . foldl scaleRowNotes [] $ ns }
    where
      scaleRowNotes :: [ScaleRowNote] -> Note -> [ScaleRowNote]
      scaleRowNotes [] n = [scaleRowNote n (n == root)]
      scaleRowNotes ms@((ScaleRowNote{marked = m}):_) n | n == root = (scaleRowNote n (not m)):ms
                                                        | otherwise = (scaleRowNote n m):ms
      
      scaleRowNote :: Note -> Bool -> ScaleRowNote
      scaleRowNote n m =
        let sh = (inScale prev n, inScale this n, inScale next n)
        in ScaleRowNote { note = n, marked = m, sharing' = sh }
    
data ProgressionStep = Step { scales :: [Scale]
                            , chords :: [Chord]
                            , editingScale :: Bool
                            , editingChord :: Bool
                            } deriving (Eq, Show)

progressionStep :: Chord -> Bool -> ProgressionStep
progressionStep c e = Step { scales = scalesForChord c
                              , chords = nub $ c : (chordsForScale $ head $ scalesForChord c)
                              , editingScale = e
                              , editingChord = False }

data State = State { quitting :: Bool
                   , progression :: [ProgressionStep]
                   , keys :: [Key]
                   , rows :: [ScaleRow]
                   , currentRow :: Int
                   } deriving Eq

data Command = MoveStep Int | RotateStep Int | ToggleScaleChord | Quit

buildKeys :: ScaleRow -> [Key]
buildKeys row = 
  map buildKey (notes row) where
    buildKey :: ScaleRowNote -> Key
    buildKey (ScaleRowNote {note = n, sharing' = sh}) = Key { keyNote = n, pressed = False, sharing = sh }

moveStep :: Int -> State -> State
moveStep delta state@State{progression = p, rows = rs} =
  let i = maybe (length p) id $ findIndex (\x -> editingScale x || editingChord x) p
      i' = max 0 $ min ((length p) - 1) $ i + delta
      p' = case (i `compare` i', splitAt (min i i') p) of
        (LT, (pre, (s@Step{editingScale = True}:s':post))) -> pre ++ (s{editingScale = False}:s'{editingScale = True}:post)
        (GT, (pre, (s':s@Step{editingScale = True}:post))) -> pre ++ (s'{editingScale = True}:s{editingScale = False}:post)
        (LT, (pre, (s@Step{editingChord = True}:s':post))) -> pre ++ (s{editingChord = False}:s'{editingChord = True}:post)
        (GT, (pre, (s':s@Step{editingChord = True}:post))) -> pre ++ (s'{editingChord = True}:s{editingChord = False}:post)
        _ -> p
  in  state{progression = p', keys = buildKeys (rs !! i'), currentRow = i'}

rotateStep :: Int -> State -> State
rotateStep delta state@State{progression = p, keys = ks} =
  let i = maybe (length p) id $ findIndex (\x -> editingScale x || editingChord x) p
      (pre, (s@Step{scales = scs, chords = chs, editingScale = es}:post)) = splitAt i p
      p' = if es
        then let scs' = rotate delta scs in pre ++ (s{scales = scs', chords = nub $ (head chs) : (chordsForScale $ head scs')}:post)
        else let chs' = rotate delta chs in pre ++ (s{chords = chs', scales = nub $ (head scs) : (scalesForChord $ head chs')}:post)
      ns = map keyNote ks
      rows' = scaleRows ns $ map (head . scales) p'
  in  state{progression = p', rows = rows', keys = buildKeys (rows' !! i)}

toggleScaleChord :: State -> State
toggleScaleChord state@State{progression = p} =
  let i = maybe (length p) id $ findIndex (\x -> editingScale x || editingChord x) p
      (pre, (s@Step{editingScale = es, editingChord = ec}:post)) = splitAt i p
  in state{progression = pre ++ (s{editingScale = not es, editingChord = not ec}:post)} where

quit :: State -> State
quit state = state{quitting = True}

handleCommand :: Command -> State -> State
handleCommand (MoveStep delta) = moveStep delta
handleCommand (RotateStep delta) = rotateStep delta
handleCommand ToggleScaleChord = toggleScaleChord
handleCommand Quit = quit