module App 
  ( Key (..)
  , ProgressionStep (..)
  , State (..)
  , SharedScaleNote (..)
  , Command (..)
  , Role (..)
  , NoteSharing (..)
  , progressionStep
  , findSharedNotes
  , markScaleSpan
  , handleCommand
  , key
  ) where

import Data.List (findIndex, nub)

import Music (Note (..), Chord (..), Scale (..), inScale, scalesForChord, chordsForScale)
import Util (rotate)

data Role = Prev | This | Next | None deriving Eq
data NoteSharing = InPrevAndThis | InThis | InThisAndNext | InAll | InNone deriving Eq

data Key = Key { keyNote :: Note
               , pressed :: Bool
               , sharing :: NoteSharing
               } deriving Eq

key :: Note -> Bool -> Key
key n pr = Key { keyNote = n, pressed = pr, sharing = InNone }

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

data SharedScaleNote = SSN { sharedNote :: Note 
                           , inPrev :: Bool
                           , inThis :: Bool
                           , inNext :: Bool
                           }

-- partOfScale :: [Note] -> Scale -> [(Note, Bool)]
-- partOfScale ns s = map (\n -> (n, inScale s n)) ns

findSharedNotes :: [Note] -> Maybe Scale -> Scale -> Maybe Scale -> [SharedScaleNote]
findSharedNotes ns prev this next = map (\n -> SSN { sharedNote = n
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

data Command = MoveStep Int | RotateStep Int | ToggleScaleChord | Quit

buildKeys :: [Note] -> [ProgressionStep] -> [Key]
buildKeys ns =
  buildKeys' . getScales . findCurrentSteps where
    findCurrentSteps :: [ProgressionStep] -> (Maybe ProgressionStep, ProgressionStep, Maybe ProgressionStep)
    findCurrentSteps steps = case steps of
                       (s1:s2@Step{editingScale = True}:s3:_) -> (Just s1, s2, Just s3)
                       (s1:s2@Step{editingChord = True}:s3:_) -> (Just s1, s2, Just s3)
                       (s1@Step{editingScale = True}:s2:_)    -> (Nothing, s1, Just s2)
                       (s1@Step{editingChord = True}:s2:_)    -> (Nothing, s1, Just s2)
                       (s1:s2@Step{editingScale = True}:[])   -> (Just s1, s2, Nothing)
                       (s1:s2@Step{editingChord = True}:[])   -> (Just s1, s2, Nothing)
                       (s1:s2:s3:ss) -> findCurrentSteps (s2:s3:ss)
                       _ -> error "invalid number of progression steps (must be at least 2)"
    
    getScales :: (Maybe ProgressionStep, ProgressionStep, Maybe ProgressionStep) -> (Maybe Scale, Scale, Maybe Scale)
    getScales (ps1, ps2, ps3) = (getScale <$> ps1, getScale ps2, getScale <$> ps3) where
      getScale = head . scales

    buildKeys' :: (Maybe Scale, Scale, Maybe Scale) -> [Key]
    buildKeys' (prev, this, next) = 
      let sharedNotes = findSharedNotes ns prev this next
      in  map buildKey sharedNotes where
        buildKey :: SharedScaleNote -> Key
        buildKey ssn =
          let sharing = case (inPrev ssn, inThis ssn, inNext ssn) of
                          (True, True, True)   -> InAll
                          (True, True, False)  -> InPrevAndThis
                          (False, True, True)  -> InThisAndNext
                          (_, True, _)         -> InThis
                          _                    -> InNone
          in Key { keyNote = sharedNote ssn, pressed = False, sharing = sharing }

moveStep :: Int -> State -> State
moveStep delta state@State{progression = p, keys = ks} =
  let i = maybe (length p) id $ findIndex (\x -> editingScale x || editingChord x) p
      i' = max 0 $ min ((length p) - 1) $ i + delta
      p' = case (i `compare` i', splitAt (min i i') p) of
        (LT, (pre, (s@Step{editingScale = True}:s':post))) -> pre ++ (s{editingScale = False}:s'{editingScale = True}:post)
        (GT, (pre, (s':s@Step{editingScale = True}:post))) -> pre ++ (s'{editingScale = True}:s{editingScale = False}:post)
        (LT, (pre, (s@Step{editingChord = True}:s':post))) -> pre ++ (s{editingChord = False}:s'{editingChord = True}:post)
        (GT, (pre, (s':s@Step{editingChord = True}:post))) -> pre ++ (s'{editingChord = True}:s{editingChord = False}:post)
        _ -> p
  in  state{progression = p', keys = buildKeys (map keyNote ks) p'}

rotateStep :: Int -> State -> State
rotateStep delta state@State{progression = p, keys = ks} =
  let i = maybe (length p) id $ findIndex (\x -> editingScale x || editingChord x) p
      (pre, (s@Step{scales = scs, chords = chs, editingScale = es}:post)) = splitAt i p
      p' = if es
        then let scs' = rotate delta scs in pre ++ (s{scales = scs', chords = nub $ (head chs) : (chordsForScale $ head scs')}:post)
        else let chs' = rotate delta chs in pre ++ (s{chords = chs', scales = nub $ (head scs) : (scalesForChord $ head chs')}:post)
  in  state{progression = p', keys = buildKeys (map keyNote ks) p'}

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