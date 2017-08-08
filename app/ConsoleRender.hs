module ConsoleRender 
  ( display
  ) where

import Control.Monad (when)
import Control.Applicative (ZipList (..), (<$>), (<*>))
import Data.Char (chr)

import System.Console.ANSI

import Music (Note (..), isRoot)
import Console (Command (..), Pos, Col (..), out, setColor)
import App (State (..), Key (..), ProgressionStep (..), Role (..), ScaleRow (..), ScaleRowNote (..))
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

upArrow :: String
upArrow = [chr 0x18]

data KeyShape = CFKey | DAKey | EBKey | GKey | BlkKey

drawKeyShape :: KeyShape -> String -> Pos -> Col -> Col -> IO ()
drawKeyShape CFKey n p fg bg = out $ Jmp p
                                    : Pn fg bg "│  "
                                    : Pn fg bg "│  "
                                    : Pn fg bg "│  "
                                    : Pn fg bg "│  "
                                    : Pn fg bg "│    "
                                    : Pn fg bg "│    "
                                    : Pn fg bg "│    "
                                    : Mv 1 (-2) : P fg bg n
                                    : []

drawKeyShape DAKey n p fg bg = out $ Jmp p : Mv 1 0
                                    : Pn fg bg   "  "
                                    : Pn fg bg   "  "
                                    : Pn fg bg   "  "
                                    : Pn fg bg   "  " : Mv (-2) 0
                                    : Pn fg bg "│    "
                                    : Pn fg bg "│ D  "
                                    : Pn fg bg "│    "
                                    : Mv 2 (-2) : P fg bg n
                                    : []

drawKeyShape EBKey n p fg bg = out $ Jmp p : Mv 1 0
                                    : Pn fg bg    "  "
                                    : Pn fg bg    "  "
                                    : Pn fg bg    "  "
                                    : Pn fg bg    "  " : Mv (-3) 0
                                    : Pn fg bg "│    "
                                    : Pn fg bg "│    "
                                    : Pn fg bg "│    "
                                    : Mv 3 (-2) : P fg bg n
                                    : []

drawKeyShape GKey n p fg bg = out $ Jmp p : Mv 1 0
                                  : Pn fg bg   "  "
                                  : Pn fg bg   "  "
                                  : Pn fg bg   "  "
                                  : Pn fg bg   "  " : Mv (-2) 0
                                  : Pn fg bg "│     "
                                  : Pn fg bg "│     "
                                  : Pn fg bg "│     "
                                  : Mv 2 (-2) : P fg bg n
                                  : []

drawKeyShape BlkKey n p fg bg = out $ Jmp p
                                    : Pn fg bg "    "
                                    : Pn fg bg "    "
                                    : Pn fg bg " Eb "
                                    : Pn fg bg "    "
                                    : Mv 1 (-2) : P fg bg n 
                                    : []

drawKeys :: Pos -> [Key] -> IO ()
drawKeys (x0, y0) ks = do
  sequence_ $ getZipList $ drawKey <$> ZipList ks
                                   <*> ZipList [(x0 + i, y0) | i <- [0, 3 ..]]
                                   <*> ZipList (True : repeat False)
  setColor (Col Vivid White) (Col Dull Black)
  setCursorPosition (y0 + 7) x0 where
    wh = (Col Vivid White)
    bl = (Col Dull Black)

    drawKey :: Key -> Pos -> Bool -> IO ()
    drawKey Key{keyNote = C, sharing = sh} p isFirst = do
      let bg = getWhiteKeyBg sh 
      drawKeyShape CFKey "C" p bl bg >> drawWhiteKeyMarker sh
      when isFirst (out $ Jmp p : (take 7 $ repeat $ Pn bl bg " "))
    drawKey Key{keyNote = Cs, sharing = sh} p _ = (drawKeyShape BlkKey "C#" p wh $ getBlackKeyBg sh) >> drawBlackKeyMarker sh
    drawKey Key{keyNote = D,  sharing = sh} p _ = (drawKeyShape DAKey  "D"  p bl $ getWhiteKeyBg sh) >> drawWhiteKeyMarker sh
    drawKey Key{keyNote = Eb, sharing = sh} p _ = (drawKeyShape BlkKey "Eb" p wh $ getBlackKeyBg sh) >> drawBlackKeyMarker sh
    drawKey Key{keyNote = E,  sharing = sh} p _ = (drawKeyShape EBKey  "E"  p bl $ getWhiteKeyBg sh) >> drawWhiteKeyMarker sh
    drawKey Key{keyNote = F,  sharing = sh} p _ = (drawKeyShape CFKey  "F"  p bl $ getWhiteKeyBg sh) >> drawWhiteKeyMarker sh
    drawKey Key{keyNote = Fs, sharing = sh} p _ = (drawKeyShape BlkKey "F#" p wh $ getBlackKeyBg sh) >> drawBlackKeyMarker sh
    drawKey Key{keyNote = G,  sharing = sh} p _ = (drawKeyShape GKey   "G"  p bl $ getWhiteKeyBg sh) >> drawWhiteKeyMarker sh
    drawKey Key{keyNote = Ab, sharing = sh} p _ = (drawKeyShape BlkKey "Ab" p wh $ getBlackKeyBg sh) >> drawBlackKeyMarker sh
    drawKey Key{keyNote = A,  sharing = sh} p _ = (drawKeyShape DAKey  "A"  p bl $ getWhiteKeyBg sh) >> drawWhiteKeyMarker sh
    drawKey Key{keyNote = Bb, sharing = sh} p _ = (drawKeyShape BlkKey "Bb" p wh $ getBlackKeyBg sh) >> drawBlackKeyMarker sh
    drawKey Key{keyNote = B,  sharing = sh} p _ = (drawKeyShape EBKey  "B"  p bl $ getWhiteKeyBg sh) >> drawWhiteKeyMarker sh

    getWhiteKeyBg :: (Bool, Bool, Bool) -> Col
    getWhiteKeyBg (True, True, False)  = (Col Vivid Green)
    getWhiteKeyBg (False, True, False) = (Col Vivid Cyan)
    getWhiteKeyBg (False, True, True)  = (Col Vivid Red)
    getWhiteKeyBg (True, True, True)   = (Col Vivid Yellow)
    getWhiteKeyBg _                    = (Col Vivid White)

    getBlackKeyBg :: (Bool, Bool, Bool) -> Col
    getBlackKeyBg (True, True, False)  = (Col Dull Green)
    getBlackKeyBg (False, True, False) = (Col Dull Cyan)
    getBlackKeyBg (False, True, True)  = (Col Dull Red)
    getBlackKeyBg (True, True, True)   = (Col Dull Yellow)
    getBlackKeyBg _                    = (Col Dull Black)

    drawWhiteKeyMarker :: (Bool, Bool, Bool) -> IO ()
    drawWhiteKeyMarker (True, True, False) = cursorBackward 1 >> cursorUp 1 >> putStr downArrow
    drawWhiteKeyMarker (False, True, True) = cursorBackward 1 >> cursorDown 1 >> putStr downArrow
    drawWhiteKeyMarker (True, True, True)  = cursorBackward 1 >> cursorUp 1 >> putStr downArrow >> cursorDown 2 >> cursorBackward 1 >> putStr downArrow
    drawWhiteKeyMarker _                   = return ()
      
    drawBlackKeyMarker :: (Bool, Bool, Bool) -> IO ()
    drawBlackKeyMarker (True, True, False) = cursorBackward 2 >> cursorUp 1 >> putStr downArrow
    drawBlackKeyMarker (False, True, True) = cursorBackward 2 >> cursorDown 1 >> putStr downArrow
    drawBlackKeyMarker (True, True, True)  = cursorBackward 2 >> cursorUp 1 >> putStr downArrow >> cursorDown 2 >> cursorBackward 1 >> putStr downArrow
    drawBlackKeyMarker _                   = return ()
getRole :: Int -> Int -> Int -> Role
getRole curr len i | i == curr = This
                   | i == (curr - 1 + len) `mod` len = Prev
                   | i == (curr + 1 + len) `mod` len = Next
                   | otherwise = None

displayNotes :: Pos -> [ScaleRow] -> Int -> IO ()
displayNotes pos scrs curr = do
  displayNotes' pos (zip scrs $ map (getRole curr $ length scrs) [0..])
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

displayArrows :: Pos -> [ScaleRow] -> Int -> IO ()
displayArrows pos scrs curr = do
  displayArrows' pos (zip scrs $ map (getRole curr $ length scrs) [0..])
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

displayRows ::Pos -> [ScaleRow] -> Int -> IO ()
displayRows pos rs curr = do
  displayNotes pos rs curr
  displayArrows pos rs curr

displayScaleNames :: Pos -> [ProgressionStep] -> IO ()
displayScaleNames (x, y) (Step{scales = s, editingScale = e}:ss) = do
  setCursorPosition y x
  clearFromCursorToLineEnd
  if e then setColor (Col Dull Black) (Col Vivid White) else setColor (Col Vivid White) (Col Dull Black)
  putStr $ show $ head s
  displayScaleNames (x, y + 2) ss
displayScaleNames _ _ = return ()

displayChordNames :: Pos -> [ProgressionStep] -> IO ()
displayChordNames (x, y) (Step{chords = c, editingChord = e}:ss) = do
  setCursorPosition y x
  clearFromCursorToLineEnd
  if e then setColor (Col Dull Black) (Col Vivid White) else setColor (Col Vivid White) (Col Dull Black)
  putStr $ show $ head c
  displayChordNames (x, y + 2) ss
displayChordNames _ _ = return ()

clearLines :: Int -> IO ()
clearLines 0 = return ()
clearLines n = clearLine >> cursorDown 1 >> clearLines (n - 1)

display :: State -> IO ()
display state = do
  drawKeys (1, 1) $ keys state
  display' (2, 9) where
    display' :: Pos -> IO ()
    display' (x, y) = do
      setCursorPosition (y - 1) x
      clearLines $ (length (rows state) + 1) * 2
      displayRows (x, y) (rows state) (currentRow state)
      displayScaleNames (x + (24*3 + 2), y) (progression state)
      displayChordNames (x + (24*3 + 27), y) (progression state)