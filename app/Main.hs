{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Music (Note (..), Scale (..), ScaleType (..), notes, inScale)
import Data.Char (chr)
import Foreign.C.Types
import System.Console.ANSI
import Control.Monad (when)
import Control.Applicative (ZipList (..), (<$>), (<*>))
import System.IO (hSetEcho, stdin, hSetBuffering, stdout, BufferMode (..))

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

type Key = (Note, Bool)
type Pos = (Int, Int)

keys :: [Key]                     --  C    C#   D    Eb   E    F    F#   G    Ab   A    Bb   B
keys = zip (take 24 $ cycle notes) $ True:True:True:True:True:True:True:True:True:True:True:False:True:(repeat False)

downArrow :: String
downArrow = [chr 0x19]
upArrow :: String
upArrow = [chr 0x18]

data KeyShape = CFKey | DAKey | EBKey | GKey | BlkKey
data Command = P Col String | Pn Col String | Mv Int Int | Jmp Pos

data Col = Col ColorIntensity Color ColorIntensity Color

defaultColor = (Col Vivid White Dull Black)

setColor :: Col -> IO ()
setColor (Col fi fc bi bc) = setSGR [ SetConsoleIntensity NormalIntensity
                                    , SetColor Foreground fi fc
                                    , SetColor Background bi bc
                                    ]

out :: [Command] -> IO ()
out = mapM_ out' where
  out' :: Command -> IO ()
  out' (P col str) = putStrMask '%' col (replace ' ' '%' str)
  out' (Pn col str) = putStrMask '%' col (replace ' ' '%' str) >> cursorBackward (length str) >> cursorDown 1
  out' (Mv x y) = cursorForward x >> cursorDown y
  out' (Jmp (x, y)) = setCursorPosition y x

  putStrMask :: Char -> Col -> String -> IO ()
  putStrMask _ _ [] = return ()
  putStrMask mask col@(Col _ _ bi bc) str = do
    setColor (Col bi bc bi bc) >> putStr masked 
    setColor col               >> putStr text
    putStrMask mask col rest where
      (masked, str') = span (== mask) str
      (text, rest) = break (== mask) str'

drawKeys :: Pos -> [Key] -> IO ()
drawKeys (x0, y0) ks = do
  sequence_ $ getZipList $ drawKey <$> ZipList ks
                                   <*> ZipList [(x0 + i, y0) | i <- [0, 3 ..]]
                                   <*> ZipList (True : repeat False)
  setColor (Col Vivid White Dull Black)
  setCursorPosition (y0 + 7) x0 where
    red = (Col Dull Black Vivid Red)
    white = (Col Dull Black Vivid White)
    darkRed = (Col Vivid White Dull Red)
    black = (Col Vivid White Dull Black)

    drawKey :: Key -> Pos -> Bool -> IO ()
    drawKey (C, pr) p isFirst = do
      let c = if pr then red else white
      drawKeyShape CFKey "C" p c
      when isFirst (out $ Jmp p : (take 7 $ repeat $ Pn c " "))
    drawKey (Cs, pr) p _ = drawKeyShape BlkKey "C#" p $ if pr then darkRed else black
    drawKey (D, pr) p _  = drawKeyShape DAKey  "D"  p $ if pr then red else white
    drawKey (Eb, pr) p _ = drawKeyShape BlkKey "Eb" p $ if pr then darkRed else black
    drawKey (E, pr) p _  = drawKeyShape EBKey  "E"  p $ if pr then red else white
    drawKey (F, pr) p _  = drawKeyShape CFKey  "F"  p $ if pr then red else white
    drawKey (Fs, pr) p _ = drawKeyShape BlkKey "F#" p $ if pr then darkRed else black
    drawKey (G, pr) p _  = drawKeyShape GKey   "G"  p $ if pr then red else white
    drawKey (Ab, pr) p _ = drawKeyShape BlkKey "Ab" p $ if pr then darkRed else black
    drawKey (A, pr) p _  = drawKeyShape DAKey  "A"  p $ if pr then red else white
    drawKey (Bb, pr) p _ = drawKeyShape BlkKey "Bb" p $ if pr then darkRed else black
    drawKey (B, pr) p _  = drawKeyShape EBKey  "B"  p $ if pr then red else white

    drawKeyShape :: KeyShape -> String -> Pos -> Col -> IO ()
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

    drawKeyShape BlkKey n p c = out $ Jmp p : Pn c "    "
                                  : Pn c "    "
                                  : Pn c " Eb "
                                  : Pn c "    "
                                  : Mv 1 (-2) : P c n 
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

displaySharedNotes :: Pos -> [Note] -> Maybe Scale -> Scale -> Maybe Scale -> IO ()
displaySharedNotes (x, y) ns prev this next = do
  setCursorPosition y x
  sequence_ $ map displayNote $ sharedNotes
  setCursorPosition (y + 1) x
  sequence_ $ map displayArrow $ sharedNotes where
    sharedNotes = zip (findSharedNotes ns prev this next) (markScaleSpan this ns)
  
    displayNote :: (SharedScaleNote, Bool) -> IO ()
    displayNote (ssn, marked) = do
      setNoteColor (ssn, marked)
      putStr (pad 3 $ show $ note ssn)
      setColor defaultColor
  
    displayArrow :: (SharedScaleNote, Bool) -> IO ()
    displayArrow (ssn, _) = do
      setColor (Col Dull Red Dull Black)
      putStr (pad 3 $ if (inThis ssn && inNext ssn) then downArrow else "")
      setColor defaultColor
  
    setNoteColor :: (SharedScaleNote, Bool) -> IO ()
    setNoteColor (ssn, marked) = case (inPrev ssn, inThis ssn, inNext ssn, marked) of
      (_, False, _, False)        -> setColor (Col Dull Black  Dull Black)
      (False, True, False, False) -> setColor (Col Vivid White Dull Black)
      (_, False, _, True)         -> setColor (Col Dull Red    Dull Red)
      (False, True, False, True)  -> setColor (Col Vivid White Dull Red)
      (True, True, _, False)      -> setColor (Col Vivid Red   Dull Black)
      (_, True, True, False)      -> setColor (Col Vivid Red   Dull Black)
      (True, True, _, True)       -> setColor (Col Vivid Red   Dull Red)
      (_, True, True, True)       -> setColor (Col Vivid Red   Dull Red)

displayCursor :: Pos -> IO ()
displayCursor (x, y) = do
  setCursorPosition y x >> setColor (Col Vivid White Vivid White) >> putStr "%" >> setColor defaultColor

clearCursor :: Pos -> IO ()
clearCursor (x, y) = do
  setCursorPosition y x >> setColor (Col Dull Black Dull Black) >> putStr "%"  >> setColor defaultColor
  
data State = State { cursor :: Pos
                   , cursorMin :: Pos
                   , cursorMax :: Pos
                   }

respondToInput' :: State -> IO ()
respondToInput' state@State {cursor = old@(x, y), cursorMin = (minx, miny), cursorMax = (maxx, maxy)} = do
  c <- getHiddenChar
  case c of
    'k' -> let new = (max (x - 1) minx, y) in (moveCursor' old new) >> (respondToInput' state{cursor = new})
    ';' -> let new = (min (x + 1) maxx, y) in (moveCursor' old new) >> (respondToInput' state{cursor = new})
    'o' -> let new = (x, max (y - 1) miny) in (moveCursor' old new) >> (respondToInput' state{cursor = new})
    'l' -> let new = (x, min (y + 1) maxy) in (moveCursor' old new) >> (respondToInput' state{cursor = new})
    'q' -> return ()
    _   -> respondToInput' state
    where
      moveCursor' :: Pos -> Pos -> IO ()
      moveCursor' old' new' = when (new' /= old') $ displayCursor new' >> clearCursor old'


main :: IO ()
main = do
  setSGR [Reset] >> clearScreen >> setCursorPosition 0 0
  drawKeys (1, 1) keys
  let progression = [(Scale G Minor), (Scale A Diminished), (Scale Bb Major), (Scale Fs Minor), (Scale A Major), (Scale C WholeTone)]
  let keyNotes = (take 24 $ cycle notes)
  displayRows keyNotes (2, 9) progression
  setCursorPosition 15 0
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  let (startX, startY) = (24 * 3 + 10, 9)
  respondToInput' State { cursor = (startX, startY)
                        , cursorMin = (startX, startY)
                        , cursorMax = (startX + 5, startY + 5)
                        }
    where
      displayRows :: [Note] -> Pos -> [Scale] -> IO ()
      displayRows ns pos (a:b:ss) = do
        displayRows' pos Nothing a (Just b) ss where
          displayRows' :: Pos -> Maybe Scale -> Scale -> Maybe Scale -> [Scale] -> IO ()
          displayRows' (x, y) ms1 s2 ms3@Nothing [] = do
            displayRow (x, y) ms1 s2 ms3
          displayRows' (x, y) ms1 s2 ms3@(Just s3) [] = do
            displayRow (x, y) ms1 s2 ms3
            displayRows' (x, y + 2) (Just s2) s3 Nothing []
          displayRows' (x, y) ms1 s2 ms3@(Just s3) (s4:scs) = do
            displayRow (x, y) ms1 s2 ms3
            displayRows' (x, y + 2) (Just s2) s3 (Just s4) scs
          displayRows' _ _ _ _ _ = return ()

          displayRow :: Pos -> Maybe Scale -> Scale -> Maybe Scale -> IO ()
          displayRow (x, y) s1 s2 s3 = do
            displaySharedNotes (x, y) ns s1 s2 s3
            setCursorPosition y (x + (24*3 + 2)) >> putStr (show s2)
      displayRows _ _ _ = return ()

      
        
          
          