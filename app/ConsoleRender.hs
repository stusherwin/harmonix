module ConsoleRender 
  ( display
  ) where

import Control.Monad (when)
import Control.Applicative (ZipList (..), (<$>), (<*>))
import Data.Char (chr)

import System.Console.ANSI

import Music (Note (..), Scale (..), isRoot)
import Console (Command (..), Pos, Col (..), out, setColor)
import App (State (..), Key (..), ProgressionStep (..), SharedScaleNote (..), Role (..), NoteSharing (..), findSharedNotes, markScaleSpan)
import Util (pad, rotate)
 
f = [ "│  │░░░░││░░░░│ │ │░░░░││░░░░││░░░│ │  │░░░░││░░░░│ │ │░░░░││░░░░││░░░│ │"
    , "│  │░C#░││░Eb░│ │ │░F#░││░Ab░││░Bb│ │  │░C#░││░Eb░│ │ │░F#░││░Ab░││░Bb│ │"
    , "│  └──┬─┘└─┬──┘ │ └──┬─┘└──┬─┘└─┬─┘ │  └──┬─┘└─┬──┘ │ └──┬─┘└──┬─┘└─┬─┘ │"
    , "│ C   │ D  │  E │F   │ G   │ A  │  B│ C   │ D  │  E │F   │ G   │ A  │  B│"
    , "└─────┴────┴────┴────┴─────┴────┴───┴─────┴────┴────┴────┴─────┴────┴───┘"
    ]
f' = "┐┌"

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

    getWhiteKeyBg InPrevAndThis = (Col Vivid Green)
    getWhiteKeyBg InThis        = (Col Vivid Cyan)
    getWhiteKeyBg InThisAndNext = (Col Vivid Red)
    getWhiteKeyBg InAll         = (Col Vivid Yellow)
    getWhiteKeyBg InNone        = (Col Vivid White)

    getBlackKeyBg InPrevAndThis = (Col Dull Green)
    getBlackKeyBg InThis        = (Col Dull Cyan)
    getBlackKeyBg InThisAndNext = (Col Dull Red)
    getBlackKeyBg InAll         = (Col Dull Yellow)
    getBlackKeyBg InNone        = (Col Dull Black)

    drawWhiteKeyMarker InNone = return ()
    drawWhiteKeyMarker InThis = return ()
    drawWhiteKeyMarker InPrevAndThis = cursorBackward 1 >> cursorUp 1 >> putStr upArrow
    drawWhiteKeyMarker InThisAndNext = cursorBackward 1 >> cursorDown 1 >> putStr downArrow
    drawWhiteKeyMarker InAll = cursorBackward 1 >> cursorUp 1 >> putStr upArrow >> cursorDown 2 >> cursorBackward 1 >> putStr downArrow
      
    drawBlackKeyMarker InNone = return ()
    drawBlackKeyMarker InThis = return ()
    drawBlackKeyMarker InPrevAndThis = cursorBackward 2 >> cursorUp 1 >> putStr upArrow
    drawBlackKeyMarker InThisAndNext = cursorBackward 2 >> cursorDown 1 >> putStr downArrow
    drawBlackKeyMarker InAll = cursorBackward 2 >> cursorUp 1 >> putStr upArrow >> cursorDown 2 >> cursorBackward 1 >> putStr downArrow

displaySharedNotes :: Pos -> [Note] -> (Scale, Scale, Scale) -> Role -> IO ()
displaySharedNotes (x, y) ns (prev, this, next) role = do
  setCursorPosition y x
  sequence_ $ map displayNote $ sharedNotes where
    sharedNotes = zip (findSharedNotes ns (prev, this, next)) (markScaleSpan this ns)
  
    displayNote :: (SharedScaleNote, Bool) -> IO ()
    displayNote (ssn, marked) = do
      let n = sharedNote ssn
      setNoteColor ssn
      putStr (pad 3 $ if inThis ssn then show $ sharedNote ssn else "")
      setColor (Col Vivid Black) (Col Dull Black)
      when (isRoot this n) $ do
        cursorBackward 4
        putStr "│"
        if marked
          then cursorBackward 1 >> cursorUp 1 >> putStr "┌" >> cursorBackward 1 >> cursorDown 2 >> putStr "└" >> cursorUp 1 >> cursorForward 3
          else cursorBackward 1 >> cursorUp 1 >> putStr "┐" >> cursorBackward 1 >> cursorDown 2 >> putStr "┘" >> cursorUp 1 >> cursorForward 3
      when marked $ cursorBackward 3 >> cursorUp 1 >> putStr "───" >> cursorBackward 3 >> cursorDown 2 >> putStr "───" >> cursorUp 1
      setColor defaultFg defaultBg
  
    setNoteColor :: SharedScaleNote -> IO ()
    setNoteColor ssn =
      let bg = Col Dull Black
          fg = case (inPrev ssn, inThis ssn, inNext ssn, role) of
                    (_, True, _, Prev)         -> Col Vivid Green
                    (_, True, _, Next)         -> Col Vivid Red
                    (False, True, False, This) -> Col Vivid Cyan
                    (True, True, False, This)  -> Col Vivid Green
                    (False, True, True, This)  -> Col Vivid Red
                    (True, True, True, This)   -> Col Vivid Yellow
                    (_, _, _, None)            -> Col Vivid White
                    _ -> bg
      in  setColor fg bg

displaySharedNoteArrows :: Pos -> [Note] -> (Scale, Scale, Scale) -> Role -> IO ()
displaySharedNoteArrows (x, y) ns (prev, this, next) role = do
  setCursorPosition (y + 1) x
  sequence_ $ map displayArrow $ sharedNotes where
    sharedNotes = zip (findSharedNotes ns (prev, this, next)) (markScaleSpan this ns)
  
    displayArrow :: (SharedScaleNote, Bool) -> IO ()
    displayArrow (ssn, _) = do
      setColor (case role of Prev -> (Col Vivid Green)
                             This -> (Col Vivid Red)
                             _ -> (Col Vivid White)) (Col Dull Black)
      if (inThis ssn && inNext ssn)
        then putStr (case role of Prev -> upArrow
                                  _ -> downArrow) >> cursorForward 2
        else cursorForward 3
      setColor defaultFg defaultBg

displayNotes :: [Note] -> Pos -> (Scale, Scale, Scale) -> [ProgressionStep] -> Int -> Role -> IO ()
displayNotes ns (x, y) (s1, s2, s3) _ 0 role = do
  displaySharedNotes (x, y) ns (s1, s2, s3) role
displayNotes ns (x, y) (s1, s2, s3) scs@(s4:_) n role = do
  displaySharedNotes (x, y) ns (s1, s2, s3) role
  displayNotes ns (x, y + 2) (s2, s3, (head $ scales s4)) (rotate 1 scs) (n - 1) $
    case (role, editingScale s4 || editingChord s4) of
      (Prev, _) -> This
      (This, _) -> Next
      (_, True) -> Prev
      _ -> None
displayNotes _ _ _ _ _ _ = return ()

displayArrows :: [Note] -> Pos -> (Scale, Scale, Scale) -> [ProgressionStep] -> Int -> Role -> IO ()
displayArrows ns (x, y) (s1, s2, s3) _ 0 role = do
  displaySharedNoteArrows (x, y) ns (s1, s2, s3) role
displayArrows ns (x, y) (s1, s2, s3) scs@(s4:_) n role = do
  displaySharedNoteArrows (x, y) ns (s1, s2, s3) role
  displayArrows ns (x, y + 2) (s2, s3, (head $ scales s4)) (rotate 1 scs) (n - 1) $
    case (role, editingScale s4 || editingChord s4) of
      (Prev, _) -> This
      (This, _) -> Next
      (_, True) -> Prev
      _ -> None
displayArrows _ _ _ _ _ _ = return ()

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
  
displayRows :: [Note] -> Pos -> [ProgressionStep] -> IO ()
displayRows ns (x, y) steps@(s1:s2:ss) = do
  displayScaleNames (x + (24*3 + 2), y) steps
  displayChordNames (x + (24*3 + 27), y) steps
  let lastStep = last steps
  displayNotes ns (x, y) ((head $ scales $ lastStep), (head $ scales s1), (head $ scales s2)) (rotate 2 steps) (length steps - 1)
    $ case (editingScale lastStep || editingChord lastStep, editingScale s1 || editingChord s1, editingScale s2 || editingChord s2) of
            (True, _, _) -> Next 
            (_, True, _) -> This 
            (_, _, True) -> Prev
            _         -> None
  displayArrows ns (x, y) ((head $ scales $ lastStep), (head $ scales s1), (head $ scales s2)) (rotate 2 steps) (length steps - 1) 
    $ case (editingScale lastStep || editingChord lastStep,editingScale s1 || editingChord s1, editingScale s2 || editingChord s2) of
            (True, _, _) -> Next
            (_, True, _) -> This 
            (_, _, True) -> Prev
            _         -> None
displayRows _ _ _ = return ()

display :: State -> IO ()
display state = do
  drawKeys (1, 1) (keys state)
  displayRows (map keyNote (keys state)) (2, 9) (progression state)