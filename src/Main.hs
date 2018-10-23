{-# LANGUAGE OverloadedStrings #-}
module Main where
import StringOfDeath
import Text.PrettyPrint


main :: IO ()
main = 
      do game (randGrid2) 


game grid = 
     do 
         show(sizedText (d grid) (cells grid))
         x <- getChar
         game (nextGrid grid) 
        








