module ConsoleRender 
  ( render
  ) where

import Control.Monad (when)
import Control.Applicative (ZipList (..), (<$>), (<*>))
import Data.Char (chr)
import System.Console.ANSI

import Music (Note (..), isRoot)
import Console (Command (..), Pos, Col (..), out, setColor)
import App (State (..), Key (..), ProgressionStep (..), Role (..), ScaleRow (..), ScaleRowNote (..), EditField (..))
import Util (pad)
 
-- [ "│  │░░░░││░░░░│ │ │░░░░││░░░░││░░░│ │  │░░░░││░░░░│ │ │░░░░││░░░░││░░░│ │"
-- , "│  │░C#░││░Eb░│ │ │░F#░││░Ab░││░Bb│ │  │░C#░││░Eb░│ │ │░F#░││░Ab░││░Bb│ │"
-- , "│  └──┬─┘└─┬──┘ │ └──┬─┘└──┬─┘└─┬─┘ │  └──┬─┘└─┬──┘ │ └──┬─┘└──┬─┘└─┬─┘ │"
-- , "│ C   │ D  │  E │F   │ G   │ A  │  B│ C   │ D  │  E │F   │ G   │ A  │  B│"
-- , "└─────┴────┴────┴────┴─────┴────┴───┴─────┴────┴────┴────┴─────┴────┴───┘"
-- , "┐┌"
-- ]

defaultFg = (Col Vivid White)
defaultBg = (Col Dull Black)

downArrow :: String
downArrow = [chr 0x19]

-- upArrow :: String
-- upArrow = [chr 0x18]

data KeyShape = CFKey | DAKey | EBKey | GKey | BlkKey
data KeyType = WhiteKey | BlackKey

renderKeyShape :: KeyShape -> String -> Pos -> (Col, Col) -> IO ()
renderKeyShape CFKey n p c = out $ Jmp p
                                    : Pn c "│  "
                                    : Pn c "│  "
                                    : Pn c "│  "
                                    : Pn c "│  "
                                    : Pn c "│    "
                                    : Pn c "│    "
                                    : Pn c "│    "
                                    : Mv 1 (-2) : P c n
                                    : []

renderKeyShape DAKey n p c = out $ Jmp p : Mv 1 0
                                    : Pn c   "  "
                                    : Pn c   "  "
                                    : Pn c   "  "
                                    : Pn c   "  " : Mv (-2) 0
                                    : Pn c "│    "
                                    : Pn c "│ D  "
                                    : Pn c "│    "
                                    : Mv 2 (-2) : P c n
                                    : []

renderKeyShape EBKey n p c = out $ Jmp p : Mv 1 0
                                    : Pn c    "  "
                                    : Pn c    "  "
                                    : Pn c    "  "
                                    : Pn c    "  " : Mv (-3) 0
                                    : Pn c "│    "
                                    : Pn c "│    "
                                    : Pn c "│    "
                                    : Mv 3 (-2) : P c n
                                    : []

renderKeyShape GKey n p c = out $ Jmp p : Mv 1 0
                                  : Pn c   "  "
                                  : Pn c   "  "
                                  : Pn c   "  "
                                  : Pn c   "  " : Mv (-2) 0
                                  : Pn c "│     "
                                  : Pn c "│     "
                                  : Pn c "│     "
                                  : Mv 2 (-2) : P c n
                                  : []

renderKeyShape BlkKey n p c = out $ Jmp p
                                    : Pn c "    "
                                    : Pn c "    "
                                    : Pn c " Eb "
                                    : Pn c "    "
                                    : Mv 1 (-2) : P c n 
                                    : []

renderKeys :: Pos -> State -> IO ()
renderKeys (x0, y0) state = do
  sequence_ $ getZipList $ renderKey <$> ZipList ks
                                     <*> ZipList [(x0 + i, y0) | i <- [0, 3 ..]]
                                     <*> ZipList (True : repeat False)
  setColor (Col Vivid White) (Col Dull Black)
  setCursorPosition (y0 + 7) x0
  where
    cr = rows state !! currentRow state
    ks = zip (keys state) (notes cr)
    ix = styleIndex cr
    renderKey :: (Key, ScaleRowNote) -> Pos -> Bool -> IO ()
    renderKey (Key{keyNote = C}, ScaleRowNote {sharing = sh}) p isFirst = do
      let c = col WhiteKey sh
      renderKeyShape CFKey "C" p c >> renderMarker WhiteKey sh
      when isFirst (out $ Jmp p : (take 7 $ repeat $ Pn c " "))
    renderKey (Key{keyNote = Cs}, ScaleRowNote {sharing = sh}) p _ = renderKeyShape BlkKey "C#" p (col BlackKey sh) >> renderMarker BlackKey sh
    renderKey (Key{keyNote = D},  ScaleRowNote {sharing = sh}) p _ = renderKeyShape DAKey  "D"  p (col WhiteKey sh) >> renderMarker WhiteKey sh
    renderKey (Key{keyNote = Eb}, ScaleRowNote {sharing = sh}) p _ = renderKeyShape BlkKey "Eb" p (col BlackKey sh) >> renderMarker BlackKey sh
    renderKey (Key{keyNote = E},  ScaleRowNote {sharing = sh}) p _ = renderKeyShape EBKey  "E"  p (col WhiteKey sh) >> renderMarker WhiteKey sh
    renderKey (Key{keyNote = F},  ScaleRowNote {sharing = sh}) p _ = renderKeyShape CFKey  "F"  p (col WhiteKey sh) >> renderMarker WhiteKey sh
    renderKey (Key{keyNote = Fs}, ScaleRowNote {sharing = sh}) p _ = renderKeyShape BlkKey "F#" p (col BlackKey sh) >> renderMarker BlackKey sh
    renderKey (Key{keyNote = G},  ScaleRowNote {sharing = sh}) p _ = renderKeyShape GKey   "G"  p (col WhiteKey sh) >> renderMarker WhiteKey sh
    renderKey (Key{keyNote = Ab}, ScaleRowNote {sharing = sh}) p _ = renderKeyShape BlkKey "Ab" p (col BlackKey sh) >> renderMarker BlackKey sh
    renderKey (Key{keyNote = A},  ScaleRowNote {sharing = sh}) p _ = renderKeyShape DAKey  "A"  p (col WhiteKey sh) >> renderMarker WhiteKey sh
    renderKey (Key{keyNote = Bb}, ScaleRowNote {sharing = sh}) p _ = renderKeyShape BlkKey "Bb" p (col BlackKey sh) >> renderMarker BlackKey sh
    renderKey (Key{keyNote = B},  ScaleRowNote {sharing = sh}) p _ = renderKeyShape EBKey  "B"  p (col WhiteKey sh) >> renderMarker WhiteKey sh
    
    col :: KeyType -> (Bool, Bool, Bool) -> (Col, Col)
    col WhiteKey (_, True, _) = (Col Dull Black, styleIndexColor Vivid ix)
    col WhiteKey _ = (Col Dull Black, Col Vivid White)
    col BlackKey (_, True, _) = (Col Vivid White, styleIndexColor Dull ix)
    col BlackKey _ = (Col Vivid White, Col Dull Black)

    renderMarker :: KeyType -> (Bool, Bool, Bool) -> IO ()
    renderMarker WhiteKey (True, True, False) = cursorBackward 1 >> cursorUp 1   >> putStr downArrow
    renderMarker WhiteKey (False, True, True) = cursorBackward 1 >> cursorDown 1 >> putStr downArrow
    renderMarker WhiteKey (True, True, True)  = cursorBackward 1 >> cursorUp 1   >> putStr downArrow >> cursorDown 2 >> cursorBackward 1 >> putStr downArrow
    renderMarker BlackKey (True, True, False) = cursorBackward 2 >> cursorUp 1   >> putStr downArrow
    renderMarker BlackKey (False, True, True) = cursorBackward 2 >> cursorDown 1 >> putStr downArrow
    renderMarker BlackKey (True, True, True)  = cursorBackward 2 >> cursorUp 1   >> putStr downArrow >> cursorDown 2 >> cursorBackward 1 >> putStr downArrow
    renderMarker _ _                   = return ()

getRole :: Int -> Int -> Int -> Role
getRole curr len i | i == curr = This
                   | i == (curr - 1 + len) `mod` len = Prev
                   | i == (curr + 1 + len) `mod` len = Next
                   | otherwise = None

styleIndexColor :: ColorIntensity -> Int -> Col
styleIndexColor int 0 = Col int Green
styleIndexColor int 1 = Col int Red
styleIndexColor int 2 = Col int Cyan
styleIndexColor int 3 = Col int Yellow
styleIndexColor _ _   = Col Dull Black

renderNotes :: Pos -> State -> IO ()
renderNotes pos state = do
  renderNotes' pos True $ zip3 prevs rs $ map (getRole (currentRow state) $ length $ rs) [0..]
  where
    rs = rows state
    prevs = last rs : rs
    renderNotes' :: Pos -> Bool -> [(ScaleRow, ScaleRow, Role)] -> IO ()
    renderNotes' _ _ [] = return ()
    renderNotes' (x, y) isFirstRow ((prev, this, role):rest) = do
      renderNoteRow (x, y) isFirstRow prev this role
      renderNotes' (x, y + 2) False rest

    renderNoteRow :: Pos -> Bool -> ScaleRow -> ScaleRow -> Role -> IO ()
    renderNoteRow (x, y) isFirstRow prev this role = do
      setCursorPosition y x
      sequence_ $ map (renderNote isFirstRow prev this role) $ (notes prev `zip` notes this)
  
    renderNote :: Bool -> ScaleRow -> ScaleRow -> Role -> (ScaleRowNote, ScaleRowNote) -> IO ()
    renderNote isFirstRow prev this role (ScaleRowNote {marked = prevMarked}, ScaleRowNote {note = n, marked = m, sharing = sh@(_, inThis, _)}) = do
      setNoteColor role sh m (styleIndex this)
      putStr (pad 3 $ if inThis then show $ n else "")
      setColor (Col Dull White) (Col Dull Black)
      when (isRoot (scale this) n) $ do
        cursorBackward 4
        putStr "│"
        cursorForward 3
      let top = case (isFirstRow, prevMarked, isRoot (scale prev) n, m, isRoot (scale this) n) of
                      (False, True, True, True, True)     -> "├"
                      (False, False, False, True, True)   -> "┌"
                      (False, True, False, True, True)    -> "┬"
                      (False, True, True, True, False)    -> "┴"
                      (False, False, False, False, True)  -> "┐"
                      (False, False, True, False, True)   -> "┤"
                      (False, True, False, False, True)   -> "┬"
                      (False, False, True, False, False)  -> "┘"
                      (False, True, True, False, False)   -> "└"
                      (False, False, True, True, False)   -> "┴"
                      (False, False, False, False, False) -> " "
                      (False, True, False, True, False)   -> "─"
                      (False, True, False, False, False)  -> "─"
                      (False, False, False, True, False)  -> "─"
                      (True, _, _, True, True)  -> "┌"
                      (True, _, _, False, True) -> "┐"
                      (True, _, _, True, False) -> "─"
                      (True, _, _, _, _)        -> " "
                      _ -> "?"
      let bottom = case (m, isRoot (scale this) n) of
                      (True, True) -> "└"
                      (False, True) -> "┘"
                      (True, False) -> "─"
                      _ -> " "
      cursorBackward 4 >> cursorUp 1 >> putStr top >> cursorBackward 1 >> cursorDown 2 >> putStr bottom >> cursorUp 1 >> cursorForward 3
      when m $ cursorBackward 3 >> cursorUp 1 >> putStr "──" >> cursorBackward 2 >> cursorDown 2 >> putStr "──" >> cursorUp 1 >> cursorForward 1
      setColor defaultFg defaultBg
  
    setNoteColor :: Role -> (Bool, Bool, Bool) -> Bool -> Int -> IO ()
    setNoteColor role (inPrev, inThis, inNext) marked i =
      let bg = Col Dull Black
          fgIn = if marked then Vivid else Dull
          fg = if inThis then styleIndexColor fgIn i else bg
      in  setColor fg bg


renderArrows :: Pos -> State -> IO ()
renderArrows pos state = do
  renderArrows' pos $ zip (rows state) $ map (getRole (currentRow state) $ length $ rows state) [0..]
  where
    renderArrows' :: Pos -> [(ScaleRow, Role)] -> IO ()
    renderArrows' _ [] = return ()
    renderArrows' (x, y) ((r, role):rs) = do
      renderArrowRow (x, y) r role
      renderArrows' (x, y + 2) rs

    renderArrowRow :: Pos -> ScaleRow -> Role -> IO ()
    renderArrowRow (x, y) row role = do
      setCursorPosition (y + 1) x
      sequence_ $ map (renderArrow role (styleIndex row)) $ notes row
      
    renderArrow :: Role -> Int -> ScaleRowNote -> IO ()
    renderArrow role i (ScaleRowNote {sharing = (_, inThis, inNext), marked = m}) = do
      let bg = Col Dull Black
          fgIn = if m then Vivid else Dull
          fg = styleIndexColor fgIn i
      setColor fg bg
      if (inThis && inNext)
        then putStr downArrow >> cursorForward 2
        else cursorForward 3
      setColor defaultFg defaultBg

renderRows ::Pos -> State -> IO ()
renderRows (x, y) state = do
  setCursorPosition (y - 1) x
  clearLines $ (length (rows state) + 1) * 2
  renderScaleNames (x + (24*3 + 2), y) state
  renderChordNames (x + (24*3 + 27), y) state
  renderNotes (x, y) state
  renderArrows (x, y) state

renderScaleNames :: Pos -> State -> IO ()
renderScaleNames pos State{progression = p, editField = e, currentRow = curr, rows = rs} =
  renderScaleNames' pos (zip [0..] p) where
    renderScaleNames' :: Pos -> [(Int, ProgressionStep)] -> IO ()
    renderScaleNames' _ [] = return ()   
    renderScaleNames' (x, y) ((i, Step{scales = s}):ss) = do
      let ix = styleIndex $ rs !! i
      setCursorPosition y x
      clearFromCursorToLineEnd
      case (i == curr, e) of
        (True, EditScale) -> setColor (Col Dull Black) (styleIndexColor Vivid ix) 
        _                 -> setColor (styleIndexColor Vivid ix) (Col Dull Black)
      putStr $ show $ head s
      renderScaleNames' (x, y + 2) ss

renderChordNames :: Pos -> State -> IO ()
renderChordNames pos State{progression = p, editField = e, currentRow = curr, rows = rs} =
  renderChordNames' pos (zip [0..] p) where
    renderChordNames' :: Pos -> [(Int, ProgressionStep)] -> IO ()
    renderChordNames' _ [] = return ()   
    renderChordNames' (x, y) ((i, Step{chords = c}):ss) = do
      let ix = styleIndex $ rs !! i
      setCursorPosition y x
      clearFromCursorToLineEnd
      case (i == curr, e) of
        (True, EditChord) -> setColor (Col Dull Black) (styleIndexColor Vivid ix)  
        _                 -> setColor (styleIndexColor Vivid ix) (Col Dull Black)
      putStr $ show $ head c
      renderChordNames' (x, y + 2) ss

clearLines :: Int -> IO ()
clearLines 0 = return ()
clearLines n = clearLine >> cursorDown 1 >> clearLines (n - 1)

render :: State -> IO ()
render state = do
  renderKeys (1, 1) state
  renderRows (2, 9) state