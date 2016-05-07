module Main where

import FizzBuzz

main :: IO ()
main = do
    s <- getLine
    putStrLn $ fizzBuzzSay s
    main
