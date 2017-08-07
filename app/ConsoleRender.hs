module ConsoleRender 
  ( display
  ) where

import Control.Monad (when)
import Control.Applicative (ZipList (..), (<$>), (<*>))
import Data.Char (chr)

import System.Console.ANSI

import Music (Note (..), Scale (..))
import Console (Command (..), Pos, Col (..), out, setColor)
import App (State (..), Key (..), ProgressionStep (..), SharedScaleNote (..), Role (..), NoteSharing (..), findSharedNotes, markScaleSpan)
import Util (pad)
 
f = [ "│  │░░░░││░░░░│ │ │░░░░││░░░░││░░░│ │  │░░░░││░░░░│ │ │░░░░││░░░░││░░░│ │"
    , "│  │░C#░││░Eb░│ │ │░F#░││░Ab░││░Bb│ │  │░C#░││░Eb░│ │ │░F#░││░Ab░││░Bb│ │"
    , "│  └──┬─┘└─┬──┘ │ └──┬─┘└──┬─┘└─┬─┘ │  └──┬─┘└─┬──┘ │ └──┬─┘└──┬─┘└─┬─┘ │"
    , "│ C   │ D  │  E │F   │ G   │ A  │  B│ C   │ D  │  E │F   │ G   │ A  │  B│"
    , "└─────┴────┴────┴────┴─────┴────┴───┴─────┴────┴────┴────┴─────┴────┴───┘"
    ]

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
      drawKeyShape CFKey "C" p bl bg
      when isFirst (out $ Jmp p : (take 7 $ repeat $ Pn bl bg " "))
    drawKey Key{keyNote = Cs, sharing = sh} p _ = drawKeyShape BlkKey "C#" p wh $ getBlackKeyBg sh
    drawKey Key{keyNote = D,  sharing = sh} p _ = drawKeyShape DAKey  "D"  p bl $ getWhiteKeyBg sh
    drawKey Key{keyNote = Eb, sharing = sh} p _ = drawKeyShape BlkKey "Eb" p wh $ getBlackKeyBg sh
    drawKey Key{keyNote = E,  sharing = sh} p _ = drawKeyShape EBKey  "E"  p bl $ getWhiteKeyBg sh
    drawKey Key{keyNote = F,  sharing = sh} p _ = drawKeyShape CFKey  "F"  p bl $ getWhiteKeyBg sh
    drawKey Key{keyNote = Fs, sharing = sh} p _ = drawKeyShape BlkKey "F#" p wh $ getBlackKeyBg sh
    drawKey Key{keyNote = G,  sharing = sh} p _ = drawKeyShape GKey   "G"  p bl $ getWhiteKeyBg sh
    drawKey Key{keyNote = Ab, sharing = sh} p _ = drawKeyShape BlkKey "Ab" p wh $ getBlackKeyBg sh
    drawKey Key{keyNote = A,  sharing = sh} p _ = drawKeyShape DAKey  "A"  p bl $ getWhiteKeyBg sh
    drawKey Key{keyNote = Bb, sharing = sh} p _ = drawKeyShape BlkKey "Bb" p wh $ getBlackKeyBg sh
    drawKey Key{keyNote = B,  sharing = sh} p _ = drawKeyShape EBKey  "B"  p bl $ getWhiteKeyBg sh

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

displaySharedNotes :: Pos -> [Note] -> Maybe Scale -> Scale -> Maybe Scale -> Role -> IO ()
displaySharedNotes (x, y) ns prev this next role = do
  setCursorPosition y x
  sequence_ $ map displayNote $ sharedNotes
  setCursorPosition (y + 1) x
  sequence_ $ map displayArrow $ sharedNotes where
    sharedNotes = zip (findSharedNotes ns prev this next) (markScaleSpan this ns)
  
    displayNote :: (SharedScaleNote, Bool) -> IO ()
    displayNote (ssn, marked) = do
      setNoteColor (ssn, marked)
      putStr (pad 3 $ if inThis ssn then show $ sharedNote ssn else "")
      setColor defaultFg defaultBg
  
    displayArrow :: (SharedScaleNote, Bool) -> IO ()
    displayArrow (ssn, _) = do
      setColor (case role of Prev -> (Col Vivid Green)
                             This -> (Col Vivid Red)
                             _ -> (Col Vivid White)) (Col Dull Black)
      putStr (pad 3 $ if (inThis ssn && inNext ssn) then (case role of Prev -> downArrow
                                                                       This -> upArrow
                                                                       _ -> "") else "")
      setColor defaultFg defaultBg
  
    setNoteColor :: (SharedScaleNote, Bool) -> IO ()
    setNoteColor (ssn, marked) =
      let bg = case (role, marked) of
                    (Prev, True) -> Col Dull Green
                    (This, True) -> Col Dull Cyan
                    (Next, True) -> Col Dull Red
                    (_, True)    -> Col Vivid Black
                    _            -> Col Dull Black
          fg = case (inPrev ssn, inThis ssn, inNext ssn, role) of
                    (_, True, _, Prev)         -> Col Vivid Green
                    (_, True, _, Next)         -> Col Vivid Red
                    (False, True, False, This) -> Col Vivid Cyan
                    (True, True, False, This)  -> Col Vivid Green
                    (False, True, True, This)  -> Col Vivid Red
                    (True, True, True, This)   -> Col Vivid Yellow
                    (_, _, _, None)            -> Col Vivid White
                    _ -> bg
      in setColor fg bg

displayRows :: [Note] -> Pos -> [ProgressionStep] -> IO ()
displayRows ns (xo, yo) (a:b:ss) = do
  displayScaleNames' (xo + (24*3 + 2), yo) (a:b:ss)
  displayChordNames' (xo + (24*3 + 27), yo) (a:b:ss)
  displayRows' (xo, yo) Nothing (head $ scales a) (Just $ head $ scales b) ss 
    $ case (editingScale a || editingChord a, editingScale b || editingChord b) of
            (True, _) -> This 
            (_, True) -> Prev
            _         -> None
    where
      displayRows' :: Pos -> Maybe Scale -> Scale -> Maybe Scale -> [ProgressionStep] -> Role -> IO ()
      displayRows' (x, y) ms1 s2 ms3@Nothing [] role = do
        displaySharedNotes (x, y) ns ms1 s2 ms3 role
      displayRows' (x, y) ms1 s2 ms3@(Just s3) [] role = do
        displaySharedNotes (x, y) ns ms1 s2 ms3 role
        displayRows' (x, y + 2) (Just s2) s3 Nothing [] $
          case role of
            Prev -> This
            This -> Next
            _ -> None
      displayRows' (x, y) ms1 s2 ms3@(Just s3) (s4:scs) role = do
        displaySharedNotes (x, y) ns ms1 s2 ms3 role
        displayRows' (x, y + 2) (Just s2) s3 (Just $ head $ scales s4) scs $
          case (role, editingScale s4 || editingChord s4) of
            (Prev, _) -> This
            (This, _) -> Next
            (_, True) -> Prev
            _ -> None
      displayRows' _ _ _ _ _ _ = return ()

      displayScaleNames' :: Pos -> [ProgressionStep] -> IO ()
      displayScaleNames' (x, y) (Step{scales = s, editingScale = e}:ss) = do
        setCursorPosition y x
        clearFromCursorToLineEnd
        if e then setColor (Col Dull Black) (Col Vivid White) else setColor (Col Vivid White) (Col Dull Black)
        putStr $ show $ head s
        displayScaleNames' (x, y + 2) ss
      displayScaleNames' _ _ = return ()

      displayChordNames' :: Pos -> [ProgressionStep] -> IO ()
      displayChordNames' (x, y) (Step{chords = c, editingChord = e}:ss) = do
        setCursorPosition y x
        clearFromCursorToLineEnd
        if e then setColor (Col Dull Black) (Col Vivid White) else setColor (Col Vivid White) (Col Dull Black)
        putStr $ show $ head c
        displayChordNames' (x, y + 2) ss
      displayChordNames' _ _ = return ()
displayRows _ _ _ = return ()

display :: State -> IO ()
display state = do
  drawKeys (1, 1) (keys state)
  displayRows (map keyNote (keys state)) (2, 9) (progression state)