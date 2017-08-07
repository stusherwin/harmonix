{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Music (Note (..), Chord (..), ChordType (..), Scale (..), ScaleType (..), notes, inScale, scalesForChord, chordsForScale)
import Data.Char (chr)
import Foreign.C.Types
import System.Console.ANSI
import Control.Monad (when)
import Control.Applicative (ZipList (..), (<$>), (<*>))
import System.IO (hSetEcho, stdin, hSetBuffering, stdout, BufferMode (..))
import Data.List (findIndex, last, nub)

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

f = [ "│  │░░░░││░░░░│ │ │░░░░││░░░░││░░░│ │  │░░░░││░░░░│ │ │░░░░││░░░░││░░░│ │"
    , "│  │░C#░││░Eb░│ │ │░F#░││░Ab░││░Bb│ │  │░C#░││░Eb░│ │ │░F#░││░Ab░││░Bb│ │"
    , "│  └──┬─┘└─┬──┘ │ └──┬─┘└──┬─┘└─┬─┘ │  └──┬─┘└─┬──┘ │ └──┬─┘└──┬─┘└─┬─┘ │"
    , "│ C   │ D  │  E │F   │ G   │ A  │  B│ C   │ D  │  E │F   │ G   │ A  │  B│"
    , "└─────┴────┴────┴────┴─────┴────┴───┴─────┴────┴────┴────┴─────┴────┴───┘"
    ]

pad :: Int -> String -> String
pad width str = str ++ (take (subtract (length str) width) $ repeat ' ')

replace :: Char -> Char -> String -> String
replace a b = map (\c -> if c == a then b else c)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate _ [x] = [x]
rotate n (x:xs) | n < 0     = rotate (n + 1) $ last xs : x : init xs
                | n > 0     = rotate (n - 1) $ xs ++ [x]
                | otherwise = x:xs

type Key = (Note, Bool)
type Pos = (Int, Int)

downArrow :: String
downArrow = [chr 0x19]
upArrow :: String
upArrow = [chr 0x18]

data KeyShape = CFKey | DAKey | EBKey | GKey | BlkKey
data Command = P Col Col String | Pn Col Col String | Mv Int Int | Jmp Pos

data Col = Col ColorIntensity Color

defaultFg = (Col Vivid White)
defaultBg = (Col Dull Black)

setColor :: Col -> Col -> IO ()
setColor (Col fi fc) (Col bi bc) = setSGR [ SetConsoleIntensity NormalIntensity
                                          , SetColor Foreground fi fc
                                          , SetColor Background bi bc
                                          ]

out :: [Command] -> IO ()
out = mapM_ out' where
  out' :: Command -> IO ()
  out' (P fg bg str) = putStrMask '%' fg bg (replace ' ' '%' str)
  out' (Pn fg bg str) = putStrMask '%' fg bg (replace ' ' '%' str) >> cursorBackward (length str) >> cursorDown 1
  out' (Mv x y) = cursorForward x >> cursorDown y
  out' (Jmp (x, y)) = setCursorPosition y x

  putStrMask :: Char -> Col -> Col -> String -> IO ()
  putStrMask _ _ _ [] = return ()
  putStrMask mask fg bg str = do
    setColor bg bg >> putStr masked 
    setColor fg bg >> putStr text
    putStrMask mask fg bg rest where
      (masked, str') = span (== mask) str
      (text, rest) = break (== mask) str'

drawKeys :: Pos -> [Key] -> IO ()
drawKeys (x0, y0) ks = do
  sequence_ $ getZipList $ drawKey <$> ZipList ks
                                   <*> ZipList [(x0 + i, y0) | i <- [0, 3 ..]]
                                   <*> ZipList (True : repeat False)
  setColor (Col Vivid White) (Col Dull Black)
  setCursorPosition (y0 + 7) x0 where
    re = (Col Vivid Red)
    wh = (Col Vivid White)
    dr = (Col Dull Red)
    bl = (Col Dull Black)

    drawKey :: Key -> Pos -> Bool -> IO ()
    drawKey (C, pr) p isFirst = do
      let bg = if pr then re else wh
      drawKeyShape CFKey "C" p bl bg
      when isFirst (out $ Jmp p : (take 7 $ repeat $ Pn bl bg " "))
    drawKey (Cs, pr) p _ = drawKeyShape BlkKey "C#" p wh $ if pr then dr else bl
    drawKey (D, pr) p _  = drawKeyShape DAKey  "D"  p bl $ if pr then re else wh
    drawKey (Eb, pr) p _ = drawKeyShape BlkKey "Eb" p wh $ if pr then dr else bl
    drawKey (E, pr) p _  = drawKeyShape EBKey  "E"  p bl $ if pr then re else wh
    drawKey (F, pr) p _  = drawKeyShape CFKey  "F"  p bl $ if pr then re else wh
    drawKey (Fs, pr) p _ = drawKeyShape BlkKey "F#" p wh $ if pr then dr else bl
    drawKey (G, pr) p _  = drawKeyShape GKey   "G"  p bl $ if pr then re else wh
    drawKey (Ab, pr) p _ = drawKeyShape BlkKey "Ab" p wh $ if pr then dr else bl
    drawKey (A, pr) p _  = drawKeyShape DAKey  "A"  p bl $ if pr then re else wh
    drawKey (Bb, pr) p _ = drawKeyShape BlkKey "Bb" p wh $ if pr then dr else bl
    drawKey (B, pr) p _  = drawKeyShape EBKey  "B"  p bl $ if pr then re else wh

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

partOfScale :: [Note] -> Scale -> [(Note, Bool)]
partOfScale ns s = map (\n -> (n, inScale s n)) ns

data SharedScaleNote = SSN { note :: Note 
                           , inPrev :: Bool
                           , inThis :: Bool
                           , inNext :: Bool
                           }

findSharedNotes :: [Note] -> Maybe Scale -> Scale -> Maybe Scale -> [SharedScaleNote]
findSharedNotes ns prev this next = map (\n -> SSN { note = n
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

data Role = Prev | This | Next | None
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
      putStr (pad 3 $ if inThis ssn then show $ note ssn else "")
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

displayCursor :: Pos -> IO ()
displayCursor (x, y) = do
  setCursorPosition y x >> setColor (Col Vivid White) (Col Vivid White) >> putStr "%" >> setColor defaultFg defaultBg

clearCursor :: Pos -> IO ()
clearCursor (x, y) = do
  setCursorPosition y x >> setColor (Col Dull Black) (Col Dull Black) >> putStr "%"  >> setColor defaultFg defaultBg
  
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

data State = State { cursor :: Pos
                   , cursorMin :: Pos
                   , cursorMax :: Pos
                   , quitting :: Bool
                   , progression :: [ProgressionStep]
                   , keys :: [Key]
                   } deriving Eq

respondToInput' :: State -> IO State
respondToInput' state = do
  c <- getHiddenChar
  let newState = case c of
                 'a' -> moveCursor' state ((subtract 1), id)
                 'd' -> moveCursor' state ((+ 1),        id)
                 'w' -> moveCursor' state (id,           (subtract 1))
                 's' -> moveCursor' state (id,           (+ 1))
                 'o' -> moveStep' state (subtract 1)
                 'l' -> moveStep' state (+ 1)
                 'k' -> rotateStep' state (- 1)
                 ';' -> rotateStep' state 1
                 '\t' -> toggleScaleChord' state
                 'q' -> state{quitting = True}
                 _   -> state
  return newState where
    moveCursor' :: State -> (Int -> Int, Int -> Int) -> State
    moveCursor' State {cursor = old', cursorMin = (minx, miny), cursorMax = (maxx, maxy)} delta =
      let new' = (min maxx $ max minx $ (fst delta) (fst old'), min maxy $ max miny $ (snd delta) (snd old'))
      in state{cursor = new'}

    moveStep' :: State -> (Int -> Int) -> State
    moveStep' State {progression = p} delta =
      let i = maybe (length p) id $ findIndex (\x -> editingScale x || editingChord x) p
          i' = (max 0 $ min ((length p) - 1) $ delta i)
      in  case (i `compare` i', splitAt (min i i') p) of
            (LT, (pre, (s@Step{editingScale = True}:s':post))) -> state{progression = pre ++ (s{editingScale = False}:s'{editingScale = True}:post)}
            (GT, (pre, (s':s@Step{editingScale = True}:post))) -> state{progression = pre ++ (s'{editingScale = True}:s{editingScale = False}:post)}
            (LT, (pre, (s@Step{editingChord = True}:s':post))) -> state{progression = pre ++ (s{editingChord = False}:s'{editingChord = True}:post)}
            (GT, (pre, (s':s@Step{editingChord = True}:post))) -> state{progression = pre ++ (s'{editingChord = True}:s{editingChord = False}:post)}
            _ -> state
    
    rotateStep' :: State -> Int -> State
    rotateStep' State {progression = p} delta =
      let i = maybe (length p) id $ findIndex (\x -> editingScale x || editingChord x) p
          (pre, (s@Step{scales = scs, chords = chs, editingScale = es}:post)) = splitAt i p
      in if es
           then let scs' = rotate delta scs in state{progression = pre ++ (s{scales = scs', chords = nub $ (head chs) : (chordsForScale $ head scs')}:post)}
           else let chs' = rotate delta chs in state{progression = pre ++ (s{chords = chs', scales = nub $ (head scs) : (scalesForChord $ head chs')}:post)}
    
    toggleScaleChord' :: State -> State
    toggleScaleChord' State {progression = p} =
      let i = maybe (length p) id $ findIndex (\x -> editingScale x || editingChord x) p
          (pre, (s@Step{editingScale = es, editingChord = ec}:post)) = splitAt i p
      in state{progression = pre ++ (s{editingScale = not es, editingChord = not ec}:post)} where

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  setSGR [Reset] >> clearScreen >> setCursorPosition 0 0
  let (startX, startY) = (24 * 3 + 30, 9)
  let initState = State { cursor = (startX, startY)
                        , cursorMin = (startX, startY)
                        , cursorMax = (startX + 5, startY + 5)
                        , quitting = False
                        , progression = [ progressionStep (Chord E Min7b5) True
                                        , progressionStep (Chord A Dom7) False
                                        , progressionStep (Chord C Min7) False
                                        , progressionStep (Chord F Dom7) False
                                        , progressionStep (Chord F Min7) False
                                        , progressionStep (Chord Bb Dom7) False
                                        , progressionStep (Chord Eb Maj7) False
                                        , progressionStep (Chord Ab Dom7sh11) False
                                        ]
                        , keys = zip (take 24 $ cycle notes) $ True:True:True:True:True:True:True:True:True:True:True:False:(repeat False)
                        }
  
  drawKeys (1, 1) (keys initState)
  display' Nothing initState
  interact' initState
    where
      interact' :: State -> IO()
      interact' state = do
        newState <- respondToInput' state
        when ((not . quitting) newState) $ do
          display' (Just state) newState
          interact' newState
      
      display' :: Maybe State -> State -> IO ()
      display' (Just old) new = do
        when (progression new /= progression old) $ displayRows (map fst (keys new)) (2, 9) (progression new)
        when (cursor new /= cursor old) $ displayCursor (cursor new) >> clearCursor (cursor old)

      display' Nothing new = do
        displayRows (map fst (keys new)) (2, 9) (progression new)
        displayCursor (cursor new)
      
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