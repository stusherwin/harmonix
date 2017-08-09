module Console
  ( Command (..)
  , Col (..)
  , Pos 
  , setColor
  , out
  , clearRect
  ) where

import System.Console.ANSI
import Util (replace)

type Pos = (Int, Int)
data Command = P (Col, Col) String | Pn (Col, Col) String | Mv Int Int | Jmp Pos
data Col = Col ColorIntensity Color

setColor :: Col -> Col -> IO ()
setColor (Col fi fc) (Col bi bc) = setSGR [ SetConsoleIntensity NormalIntensity
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

  putStrMask :: Char -> (Col, Col) -> String -> IO ()
  putStrMask _ _ [] = return ()
  putStrMask mask (fg, bg) str = do
    setColor bg bg >> putStr masked 
    setColor fg bg >> putStr text
    putStrMask mask (fg, bg) rest where
      (masked, str') = span (== mask) str
      (text, rest) = break (== mask) str'

clearRect :: Pos -> (Int, Int) -> IO ()
clearRect (x, y) rect = setCursorPosition y x >> clearRect' rect >> setCursorPosition y x
  where
    clearRect' (_, 0) = return ()
    clearRect' (len, n) = do
      putStr $ take len $ repeat ' '
      cursorDown 1
      clearRect' (len, n -1)
