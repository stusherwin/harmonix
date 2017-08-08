module Util
  ( pad
  , replace
  , rotate 
  , update
  ) where

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

update :: Int -> a -> [a] -> [a]
update 0 x' (_:xs) = x':xs
update _ _ [] = []
update n x' (y:xs) = y : update (n - 1) x' xs