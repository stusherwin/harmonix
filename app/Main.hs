{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Data.Char (chr)
import Foreign.C.Types
import System.Console.ANSI
import Control.Monad (when)
import System.IO (hSetEcho, stdin, hSetBuffering, stdout, BufferMode (..))
import Data.List (findIndex, nub)

import Music (Note (..), Chord (..), ChordType (..), notes, scalesForChord, chordsForScale)
import Util (rotate)
import ConsoleRender (drawKeys, displayRows)
import App

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

f = [ "│  │░░░░││░░░░│ │ │░░░░││░░░░││░░░│ │  │░░░░││░░░░│ │ │░░░░││░░░░││░░░│ │"
    , "│  │░C#░││░Eb░│ │ │░F#░││░Ab░││░Bb│ │  │░C#░││░Eb░│ │ │░F#░││░Ab░││░Bb│ │"
    , "│  └──┬─┘└─┬──┘ │ └──┬─┘└──┬─┘└─┬─┘ │  └──┬─┘└─┬──┘ │ └──┬─┘└──┬─┘└─┬─┘ │"
    , "│ C   │ D  │  E │F   │ G   │ A  │  B│ C   │ D  │  E │F   │ G   │ A  │  B│"
    , "└─────┴────┴────┴────┴─────┴────┴───┴─────┴────┴────┴────┴─────┴────┴───┘"
    ]
  
respondToInput' :: State -> IO State
respondToInput' state = do
  c <- getHiddenChar
  let newState = case c of
                 'o' -> moveStep' state (subtract 1)
                 'l' -> moveStep' state (+ 1)
                 'k' -> rotateStep' state (- 1)
                 ';' -> rotateStep' state 1
                 '\t' -> toggleScaleChord' state
                 'q' -> state{quitting = True}
                 _   -> state
  return newState where
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

initState :: State
initState = State { quitting = False
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
  
main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  setSGR [Reset] >> clearScreen >> setCursorPosition 0 0
  
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

      display' Nothing new = do
        displayRows (map fst (keys new)) (2, 9) (progression new)