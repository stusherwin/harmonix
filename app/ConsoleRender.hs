module ConsoleRender 
  ( render
  ) where

import Control.Monad (when)
import Control.Applicative (ZipList (..), (<$>), (<*>))
import Data.Char (chr)
import System.Console.ANSI

import Music (Note (..), isRoot)
import Console (Command (..), Pos, Col (..), out, setColor)
import App (State (..), Key (..), ProgressionStep (..), ScaleRow (..), ScaleRowNote (..), EditField (..), SharedNoteDisplay (..))
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
    disp = sharedNoteDisplay $ progression state !! currentRow state
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
    col WhiteKey sh = (Col Dull Black,  noteColor disp ix sh Vivid (Col Vivid White))
    col BlackKey sh = (Col Vivid White, noteColor disp ix sh Dull (Col Dull Black))

    renderMarker :: KeyType -> (Bool, Bool, Bool) -> IO ()
    renderMarker WhiteKey (True, True, False) = cursorBackward 1 >> cursorUp 1   >> putStr downArrow
    renderMarker WhiteKey (False, True, True) = cursorBackward 1 >> cursorDown 1 >> putStr downArrow
    renderMarker WhiteKey (True, True, True)  = cursorBackward 1 >> cursorUp 1   >> putStr downArrow >> cursorDown 2 >> cursorBackward 1 >> putStr downArrow
    renderMarker BlackKey (True, True, False) = cursorBackward 2 >> cursorUp 1   >> putStr downArrow
    renderMarker BlackKey (False, True, True) = cursorBackward 2 >> cursorDown 1 >> putStr downArrow
    renderMarker BlackKey (True, True, True)  = cursorBackward 2 >> cursorUp 1   >> putStr downArrow >> cursorDown 2 >> cursorBackward 1 >> putStr downArrow
    renderMarker _ _                   = return ()

styleIndexColor :: ColorIntensity -> Int -> Col
styleIndexColor int 0 = Col int Green
styleIndexColor int 1 = Col int Cyan
styleIndexColor int 2 = Col int Red
styleIndexColor int 3 = Col int Yellow
styleIndexColor _ _   = Col Dull Black

noteColor :: SharedNoteDisplay -> Int -> (Bool, Bool, Bool) -> ColorIntensity -> Col -> Col
noteColor ThisOnly     ix (_,    True, _)    int _ = styleIndexColor int ix
noteColor Prev         ix (True, True, _)    int _ = styleIndexColor int $ (ix - 1) `mod` 4
noteColor Prev         ix (_,    True, _)    int _ = styleIndexColor int ix
noteColor Next         ix (_,    True, True) int _ = styleIndexColor int $ (ix + 1) `mod` 4
noteColor Next         ix (_,    True, _)    int _ = styleIndexColor int ix
noteColor PrevOverNext ix (True, True, True) int _ = styleIndexColor int $ (ix - 1) `mod` 4
noteColor PrevOverNext ix (_,    True, True) int _ = styleIndexColor int $ (ix + 1) `mod` 4
noteColor PrevOverNext ix (_,    True, _)    int _ = styleIndexColor int ix
noteColor NextOverPrev ix (True, True, True) int _ = styleIndexColor int $ (ix + 1) `mod` 4
noteColor NextOverPrev ix (True, True, _)    int _ = styleIndexColor int $ (ix - 1) `mod` 4
noteColor NextOverPrev ix (_,    True, _)    int _ = styleIndexColor int ix
noteColor _ _ _ _ def = def

renderNotes :: Pos -> State -> IO ()
renderNotes pos state = do
  renderNotes' pos True $ zip prevs rs
  where
    rs = zip (rows state) (map sharedNoteDisplay $ progression state)
    prevs = last rs : rs
    renderNotes' :: Pos -> Bool -> [((ScaleRow, SharedNoteDisplay), (ScaleRow, SharedNoteDisplay))] -> IO ()
    renderNotes' _ _ [] = return ()
    renderNotes' (x, y) isFirstRow (((prev, _), (this, disp)):rest) = do
      renderNoteRow (x, y) isFirstRow prev this disp
      renderNotes' (x, y + 2) False rest

    renderNoteRow :: Pos -> Bool -> ScaleRow -> ScaleRow -> SharedNoteDisplay -> IO ()
    renderNoteRow (x, y) isFirstRow prev this disp = do
      setCursorPosition y x
      sequence_ $ map (renderNote isFirstRow prev this disp) $ (notes prev `zip` notes this)
  
    renderNote :: Bool -> ScaleRow -> ScaleRow -> SharedNoteDisplay -> (ScaleRowNote, ScaleRowNote) -> IO ()
    renderNote isFirstRow prev this disp (ScaleRowNote {marked = prevMarked}, ScaleRowNote {note = n, marked = m, sharing = sh@(_, inThis, _)}) = do
      setNoteColor sh m (styleIndex this) disp
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
  
    setNoteColor :: (Bool, Bool, Bool) -> Bool -> Int -> SharedNoteDisplay -> IO ()
    setNoteColor sh marked i disp =
      let bg   = Col Dull Black
          fgIn = if marked then Vivid else Dull
          fg   = noteColor disp i sh fgIn bg
      in  setColor fg bg

renderArrows :: Pos -> State -> IO ()
renderArrows pos state = do
  renderArrows' pos $ rows state
  where
    renderArrows' :: Pos -> [ScaleRow] -> IO ()
    renderArrows' _ [] = return ()
    renderArrows' (x, y) (r:rs) = do
      renderArrowRow (x, y) r
      renderArrows' (x, y + 2) rs

    renderArrowRow :: Pos -> ScaleRow -> IO ()
    renderArrowRow (x, y) row = do
      setCursorPosition (y + 1) x
      sequence_ $ map (renderArrow (styleIndex row)) $ notes row
      
    renderArrow :: Int -> ScaleRowNote -> IO ()
    renderArrow i (ScaleRowNote {sharing = (_, inThis, inNext), marked = m}) = do
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