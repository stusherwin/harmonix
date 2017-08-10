module App 
  ( Key (..)
  , ProgressionStep (..)
  , State (..)
  , Command (..)
  , Role (..)
  , ScaleRow (..)
  , ScaleRowNote (..)
  , EditField (..)
  , SharedNoteDisplay (..)
  , progressionStep
  , handleCommand
  , key
  , scaleRows
  , buildKeys
  ) where

import Data.List (nub)

import Music (Note (..), Chord (..), Scale (..), inScale, scalesForChord, chordsForScale)
import Util (rotate, update)

data Role = XXX deriving Eq -- Prev | This | Next | None deriving Eq

data Key = Key { keyNote :: Note
               , pressed :: Bool
               } deriving (Eq, Show)

key :: Note -> Bool -> Key
key n pr = Key { keyNote = n, pressed = pr }

data ScaleRowNote = ScaleRowNote { note :: Note
                                 , marked :: Bool
                                 , sharing :: (Bool, Bool, Bool)
                                 } deriving (Eq, Show)

data ScaleRow = ScaleRow { scale :: Scale
                         , notes :: [ScaleRowNote]
                         , styleIndex :: Int
                         } deriving (Eq, Show)

scaleRows :: [Note] -> [Scale] -> [ScaleRow]
scaleRows ns scs = scaleRows' (zip (last scs:scs) (cycle [0..3])) (length scs) where
  scaleRows' :: [(Scale, Int)] -> Int -> [ScaleRow]
  scaleRows' [] _       = error "Not enough scales"
  scaleRows' (_:[]) _   = error "Not enough scales"
  scaleRows' (_:_:[]) _ = error "Not enough scales"
  scaleRows' _ 0        = []
  scaleRows' ss@(s1:s2:s3:_) n = scaleRow (s1, s2, s3) : scaleRows' (rotate 1 ss) (n - 1)

  scaleRow :: ((Scale, Int), (Scale, Int), (Scale, Int)) -> ScaleRow
  scaleRow ((prev, _), (this@(Scale root _), i), (next, _)) = 
    ScaleRow { scale = this
             , notes = reverse . foldl scaleRowNotes [] $ ns 
             , styleIndex = i
             }
    where
      scaleRowNotes :: [ScaleRowNote] -> Note -> [ScaleRowNote]
      scaleRowNotes [] n = [scaleRowNote n (n == root)]
      scaleRowNotes ms@((ScaleRowNote{marked = m}):_) n | n == root = (scaleRowNote n (not m)):ms
                                                        | otherwise = (scaleRowNote n m):ms
      
      scaleRowNote :: Note -> Bool -> ScaleRowNote
      scaleRowNote n m =
        let sh = (inScale prev n, inScale this n, inScale next n)
        in ScaleRowNote { note = n, marked = m, sharing = sh }
    
data ProgressionStep = Step { scales :: [Scale]
                            , chords :: [Chord]
                            } deriving (Eq, Show)

progressionStep :: Chord -> ProgressionStep
progressionStep c = Step { scales = scalesForChord c
                         , chords = nub $ c : (chordsForScale $ head $ scalesForChord c)
                         }

data EditField = EditScale | EditChord deriving Eq

data SharedNoteDisplay = ThisOnly | Prev | Next | PrevOverNext | NextOverPrev deriving Eq

data State = State { quitting :: Bool
                   , progression :: [ProgressionStep]
                   , keys :: [Key]
                   , rows :: [ScaleRow]
                   , currentRow :: Int
                   , editField :: EditField
                   , sharedNoteDisplay :: SharedNoteDisplay
                   } deriving Eq

data Command = MoveStep Int | RotateStep Int | ToggleScaleChord | TogglePrev | ToggleNext | Quit

buildKeys :: ScaleRow -> [Key]
buildKeys row = 
  map buildKey (notes row) where
    buildKey :: ScaleRowNote -> Key
    buildKey (ScaleRowNote {note = n}) = Key { keyNote = n, pressed = False }

moveStep :: Int -> State -> State
moveStep delta state@State{progression = p, rows = rs, currentRow = i} =
  let i' = (i + delta) `mod` (length p)
  in  state{currentRow = i', keys = buildKeys (rs !! i')}

rotateStep :: Int -> State -> State
rotateStep delta state@State{progression = p, keys = ks, currentRow = i, editField = ef} =
  let s@Step{scales = scs, chords = chs} = p !! i
      p' = case ef of
             EditScale -> let scs' = rotate delta scs 
                              s' = s{scales = scs', chords = nub $ (head chs) : (chordsForScale $ head scs')}
                          in  update i s' p
             EditChord -> let chs' = rotate delta chs 
                              s' = s{chords = chs', scales = nub $ (head scs) : (scalesForChord $ head chs')}
                          in  update i s' p
      ns = map keyNote ks
      rows' = scaleRows ns $ map (head . scales) p'
  in  state{progression = p', rows = rows', keys = buildKeys (rows' !! i)}

toggleScaleChord :: State -> State
toggleScaleChord state@State{editField = ef} =
  let ef' = case ef of EditScale -> EditChord
                       _ -> EditScale
  in state{editField = ef'}

togglePrev :: State -> State
togglePrev s@State{sharedNoteDisplay = ThisOnly}      = s{sharedNoteDisplay = Prev}
togglePrev s@State{sharedNoteDisplay = Prev}          = s{sharedNoteDisplay = ThisOnly}
togglePrev s@State{sharedNoteDisplay = Next}          = s{sharedNoteDisplay = NextOverPrev}
togglePrev s@State{sharedNoteDisplay = PrevOverNext}  = s{sharedNoteDisplay = Next}
togglePrev s@State{sharedNoteDisplay = NextOverPrev}  = s{sharedNoteDisplay = Next}

toggleNext :: State -> State
toggleNext s@State{sharedNoteDisplay = ThisOnly}      = s{sharedNoteDisplay = Next}
toggleNext s@State{sharedNoteDisplay = Prev}          = s{sharedNoteDisplay = PrevOverNext}
toggleNext s@State{sharedNoteDisplay = Next}          = s{sharedNoteDisplay = ThisOnly}
toggleNext s@State{sharedNoteDisplay = PrevOverNext}  = s{sharedNoteDisplay = Prev}
toggleNext s@State{sharedNoteDisplay = NextOverPrev}  = s{sharedNoteDisplay = Prev}

quit :: State -> State
quit state = state{quitting = True}

handleCommand :: Command -> State -> State
handleCommand (MoveStep delta) = moveStep delta
handleCommand (RotateStep delta) = rotateStep delta
handleCommand ToggleScaleChord = toggleScaleChord
handleCommand TogglePrev = togglePrev
handleCommand ToggleNext = toggleNext
handleCommand Quit = quit