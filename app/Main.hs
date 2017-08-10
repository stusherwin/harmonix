{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Data.Char (chr)
import Foreign.C.Types
import System.Console.ANSI
import Control.Monad (when)
import System.IO (hSetEcho, stdin, hSetBuffering, stdout, BufferMode (..))

import Music (Note (..), Chord (..), ChordType (..), notes)
import ConsoleRender (render)
import App (State (..), Command (..), ProgressionStep (..), EditField (..), SharedNoteDisplay (..), progressionStep, handleCommand, key, scaleRows)

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

initState :: State
initState = State { quitting = False
                  , progression = steps
                  , rows = scaleRows ns $ map (head . scales) steps
                  , currentRow = 0
                  , editField = EditScale
                  , keys = zipWith key ns pressed
                  } where ns = take 24 $ cycle notes
                          steps = [ progressionStep (Chord E Min7b5)
                                  , progressionStep (Chord A Dom7)
                                  , progressionStep (Chord C Min7)
                                  , progressionStep (Chord F Dom7)
                                  , progressionStep (Chord F Min7)
                                  , progressionStep (Chord Bb Dom7)
                                  , progressionStep (Chord Eb Maj7)
                                  , progressionStep (Chord Ab Dom7sh11)
                                  ]
                          pressed = True:True:True:True:True:True:True:True:True:True:True:False:(repeat False)
  
main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  setSGR [Reset] >> clearScreen >> setCursorPosition 0 0
  render initState
  inputLoop initState
    where
      inputLoop :: State -> IO()
      inputLoop state = do
        c <- getHiddenChar
        let command = case c of
                      'o' ->  Just $ MoveStep (-1)
                      'l' ->  Just $ MoveStep 1
                      'k' ->  Just $ RotateStep (-1)
                      ';' ->  Just $ RotateStep 1
                      '\t' -> Just $ ToggleScaleChord
                      'p' ->  Just $ TogglePrev
                      'n' ->  Just $ ToggleNext
                      'q' ->  Just $ Quit
                      _   ->  Nothing
        let state' = maybe state (\com -> handleCommand com state) command
        when ((not . quitting) state') $ do
          when (state' /= state) (render state')
          inputLoop state'