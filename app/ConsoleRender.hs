module ConsoleRender 
  ( display
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

drawKeyShape :: KeyShape -> String -> Pos -> (Col, Col) -> IO ()
drawKeyShape CFKey n p c = out $ Jmp p
                                    : Pn c "│  "
                                    : Pn c "│  "
                                    : Pn c "│  "
                                    : Pn c "│  "
                                    : Pn c "│    "
                                    : Pn c "│    "
                                    : Pn c "│    "
                                    : Mv 1 (-2) : P c n
                                    : []

drawKeyShape DAKey n p c = out $ Jmp p : Mv 1 0
                                    : Pn c   "  "
                                    : Pn c   "  "
                                    : Pn c   "  "
                                    : Pn c   "  " : Mv (-2) 0
                                    : Pn c "│    "
                                    : Pn c "│ D  "
                                    : Pn c "│    "
                                    : Mv 2 (-2) : P c n
                                    : []

drawKeyShape EBKey n p c = out $ Jmp p : Mv 1 0
                                    : Pn c    "  "
                                    : Pn c    "  "
                                    : Pn c    "  "
                                    : Pn c    "  " : Mv (-3) 0
                                    : Pn c "│    "
                                    : Pn c "│    "
                                    : Pn c "│    "
                                    : Mv 3 (-2) : P c n
                                    : []

drawKeyShape GKey n p c = out $ Jmp p : Mv 1 0
                                  : Pn c   "  "
                                  : Pn c   "  "
                                  : Pn c   "  "
                                  : Pn c   "  " : Mv (-2) 0
                                  : Pn c "│     "
                                  : Pn c "│     "
                                  : Pn c "│     "
                                  : Mv 2 (-2) : P c n
                                  : []

drawKeyShape BlkKey n p c = out $ Jmp p
                                    : Pn c "    "
                                    : Pn c "    "
                                    : Pn c " Eb "
                                    : Pn c "    "
                                    : Mv 1 (-2) : P c n 
                                    : []

drawKeys :: Pos -> [Key] -> IO ()
drawKeys (x0, y0) ks = do
  sequence_ $ getZipList $ drawKey <$> ZipList ks
                                   <*> ZipList [(x0 + i, y0) | i <- [0, 3 ..]]
                                   <*> ZipList (True : repeat False)
  setColor (Col Vivid White) (Col Dull Black)
  setCursorPosition (y0 + 7) x0 where
    drawKey :: Key -> Pos -> Bool -> IO ()
    drawKey Key{keyNote = C, sharing = sh} p isFirst = do
      let c = col WhiteKey sh 
      drawKeyShape CFKey "C" p c >> drawMarker WhiteKey sh
      when isFirst (out $ Jmp p : (take 7 $ repeat $ Pn c " "))
    drawKey Key{keyNote = Cs, sharing = sh} p _ = drawKeyShape BlkKey "C#" p (col BlackKey sh) >> drawMarker BlackKey sh
    drawKey Key{keyNote = D,  sharing = sh} p _ = drawKeyShape DAKey  "D"  p (col WhiteKey sh) >> drawMarker WhiteKey sh
    drawKey Key{keyNote = Eb, sharing = sh} p _ = drawKeyShape BlkKey "Eb" p (col BlackKey sh) >> drawMarker BlackKey sh
    drawKey Key{keyNote = E,  sharing = sh} p _ = drawKeyShape EBKey  "E"  p (col WhiteKey sh) >> drawMarker WhiteKey sh
    drawKey Key{keyNote = F,  sharing = sh} p _ = drawKeyShape CFKey  "F"  p (col WhiteKey sh) >> drawMarker WhiteKey sh
    drawKey Key{keyNote = Fs, sharing = sh} p _ = drawKeyShape BlkKey "F#" p (col BlackKey sh) >> drawMarker BlackKey sh
    drawKey Key{keyNote = G,  sharing = sh} p _ = drawKeyShape GKey   "G"  p (col WhiteKey sh) >> drawMarker WhiteKey sh
    drawKey Key{keyNote = Ab, sharing = sh} p _ = drawKeyShape BlkKey "Ab" p (col BlackKey sh) >> drawMarker BlackKey sh
    drawKey Key{keyNote = A,  sharing = sh} p _ = drawKeyShape DAKey  "A"  p (col WhiteKey sh) >> drawMarker WhiteKey sh
    drawKey Key{keyNote = Bb, sharing = sh} p _ = drawKeyShape BlkKey "Bb" p (col BlackKey sh) >> drawMarker BlackKey sh
    drawKey Key{keyNote = B,  sharing = sh} p _ = drawKeyShape EBKey  "B"  p (col WhiteKey sh) >> drawMarker WhiteKey sh
    
    col :: KeyType -> (Bool, Bool, Bool) -> (Col, Col)
    col WhiteKey (True, True, False)  = (Col Dull Black,  Col Vivid Green)
    col WhiteKey (False, True, False) = (Col Dull Black,  Col Vivid Cyan)
    col WhiteKey (False, True, True)  = (Col Dull Black,  Col Vivid Red)
    col WhiteKey (True, True, True)   = (Col Dull Black,  Col Vivid Yellow)
    col WhiteKey _                    = (Col Dull Black,  Col Vivid White)
    col BlackKey (True, True, False)  = (Col Vivid White, Col Dull Green)
    col BlackKey (False, True, False) = (Col Vivid White, Col Dull Cyan)
    col BlackKey (False, True, True)  = (Col Vivid White, Col Dull Red)
    col BlackKey (True, True, True)   = (Col Vivid White, Col Dull Yellow)
    col BlackKey _                    = (Col Vivid White, Col Dull Black)

    drawMarker :: KeyType -> (Bool, Bool, Bool) -> IO ()
    drawMarker WhiteKey (True, True, False) = cursorBackward 1 >> cursorUp 1   >> putStr downArrow
    drawMarker WhiteKey (False, True, True) = cursorBackward 1 >> cursorDown 1 >> putStr downArrow
    drawMarker WhiteKey (True, True, True)  = cursorBackward 1 >> cursorUp 1   >> putStr downArrow >> cursorDown 2 >> cursorBackward 1 >> putStr downArrow
    drawMarker BlackKey (True, True, False) = cursorBackward 2 >> cursorUp 1   >> putStr downArrow
    drawMarker BlackKey (False, True, True) = cursorBackward 2 >> cursorDown 1 >> putStr downArrow
    drawMarker BlackKey (True, True, True)  = cursorBackward 2 >> cursorUp 1   >> putStr downArrow >> cursorDown 2 >> cursorBackward 1 >> putStr downArrow
    drawMarker _ _                   = return ()

getRole :: Int -> Int -> Int -> Role
getRole curr len i | i == curr = This
                   | i == (curr - 1 + len) `mod` len = Prev
                   | i == (curr + 1 + len) `mod` len = Next
                   | otherwise = None

displayNotes :: Pos -> State -> IO ()
displayNotes pos state = do
  displayNotes' pos (zip (rows state) $ map (getRole (currentRow state) $ length $ rows state) [0..])
  where
    displayNotes' :: Pos -> [(ScaleRow, Role)] -> IO ()
    displayNotes' _ [] = return ()
    displayNotes' (x, y) ((r, role):rs) = do
      displayNoteRow (x, y) r role
      displayNotes' (x, y + 2) rs

    displayNoteRow :: Pos -> ScaleRow -> Role -> IO ()
    displayNoteRow (x, y) row role = do
      setCursorPosition y x
      sequence_ $ map (displayNote row role) $ notes row
  
    displayNote :: ScaleRow -> Role -> ScaleRowNote -> IO ()
    displayNote this role (ScaleRowNote {note = n, marked = m, sharing' = sh@(_, inThis, _)}) = do
      setNoteColor role sh
      putStr (pad 3 $ if inThis then show $ n else "")
      setColor (Col Vivid Black) (Col Dull Black)
      when (isRoot (scale this) n) $ do
        cursorBackward 4
        putStr "│"
        if m
          then cursorBackward 1 >> cursorUp 1 >> putStr "┌" >> cursorBackward 1 >> cursorDown 2 >> putStr "└" >> cursorUp 1 >> cursorForward 3
          else cursorBackward 1 >> cursorUp 1 >> putStr "┐" >> cursorBackward 1 >> cursorDown 2 >> putStr "┘" >> cursorUp 1 >> cursorForward 3
      when m $ cursorBackward 3 >> cursorUp 1 >> putStr "───" >> cursorBackward 3 >> cursorDown 2 >> putStr "───" >> cursorUp 1
      setColor defaultFg defaultBg
  
    setNoteColor :: Role -> (Bool, Bool, Bool) -> IO ()
    setNoteColor role (inPrev, inThis, inNext) =
      let bg = Col Dull Black
          fg = case (inPrev, inThis, inNext, role) of
                    (_, True, _, Prev)         -> Col Vivid Green
                    (_, True, _, Next)         -> Col Vivid Red
                    (False, True, False, This) -> Col Vivid Cyan
                    (True, True, False, This)  -> Col Vivid Green
                    (False, True, True, This)  -> Col Vivid Red
                    (True, True, True, This)   -> Col Vivid Yellow
                    (_, _, _, None)            -> Col Vivid White
                    _ -> bg
      in  setColor fg bg

displayArrows :: Pos -> State -> IO ()
displayArrows pos state = do
  displayArrows' pos (zip (rows state) $ map (getRole (currentRow state) $ length $ rows state) [0..])
  where
    displayArrows' :: Pos -> [(ScaleRow, Role)] -> IO ()
    displayArrows' _ [] = return ()
    displayArrows' (x, y) ((r, role):rs) = do
      displayArrowRow (x, y) r role
      displayArrows' (x, y + 2) rs

    displayArrowRow :: Pos -> ScaleRow -> Role -> IO ()
    displayArrowRow (x, y) row role = do
      setCursorPosition (y + 1) x
      sequence_ $ map (displayArrow role) $ notes row
      
    displayArrow :: Role -> ScaleRowNote -> IO ()
    displayArrow role (ScaleRowNote {sharing' = (_, inThis, inNext)}) = do
      setColor (case role of Prev -> (Col Vivid Green)
                             This -> (Col Vivid Red)
                             _ -> (Col Vivid White)) (Col Dull Black)
      if (inThis && inNext)
        then putStr downArrow >> cursorForward 2
        else cursorForward 3
      setColor defaultFg defaultBg

displayRows ::Pos -> State -> IO ()
displayRows (x, y) state = do
  setCursorPosition (y - 1) x
  clearLines $ (length (rows state) + 1) * 2
  displayScaleNames (x + (24*3 + 2), y) state
  displayChordNames (x + (24*3 + 27), y) state
  displayNotes (x, y) state
  displayArrows (x, y) state

displayScaleNames :: Pos -> State -> IO ()
displayScaleNames pos State{progression = p, editField = e, currentRow = curr} =
  displayScaleNames' pos (zip [0..] p) where
    displayScaleNames' :: Pos -> [(Int, ProgressionStep)] -> IO ()
    displayScaleNames' _ [] = return ()   
    displayScaleNames' (x, y) ((i, Step{scales = s}):ss) = do
      setCursorPosition y x
      clearFromCursorToLineEnd
      case (i == curr, e) of
        (True, EditScale) -> setColor (Col Dull Black) (Col Vivid White) 
        _                 -> setColor (Col Vivid White) (Col Dull Black)
      putStr $ show $ head s
      displayScaleNames' (x, y + 2) ss

displayChordNames :: Pos -> State -> IO ()
displayChordNames pos State{progression = p, editField = e, currentRow = curr} =
  displayChordNames' pos (zip [0..] p) where
    displayChordNames' :: Pos -> [(Int, ProgressionStep)] -> IO ()
    displayChordNames' _ [] = return ()   
    displayChordNames' (x, y) ((i, Step{chords = c}):ss) = do
      setCursorPosition y x
      clearFromCursorToLineEnd
      case (i == curr, e) of
        (True, EditChord) -> setColor (Col Dull Black) (Col Vivid White) 
        _                 -> setColor (Col Vivid White) (Col Dull Black)
      putStr $ show $ head c
      displayChordNames' (x, y + 2) ss

clearLines :: Int -> IO ()
clearLines 0 = return ()
clearLines n = clearLine >> cursorDown 1 >> clearLines (n - 1)

display :: State -> IO ()
display state = do
  drawKeys (1, 1) $ keys state
  displayRows (2, 9) state