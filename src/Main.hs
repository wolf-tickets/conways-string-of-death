module Main where
import StringOfDeath
import Text.PrettyPrint
import Data.Char
import System.Console.ANSI

main :: IO ()
main = 
      do game (randGrid) 


game :: GameGrid -> IO()
game grid = 
     do
       let fGrid = splitList (d grid) (cells grid)
       clearScreen
       mapM_ putStrLn fGrid
       _ <- getChar
       game (nextGrid grid)


splitList d [] = return [] 
splitList d cells = fst l:(splitList d (snd l))		 
                     where l = splitAt d cells






