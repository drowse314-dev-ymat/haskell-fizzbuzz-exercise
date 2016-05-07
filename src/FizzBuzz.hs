module FizzBuzz
    ( FizzBuzz
    , fizzBuzz
    , fizzBuzzSay
    ) where

import Control.Monad

data FizzBuzz = Fizz
              | Buzz deriving Show

fizzBuzz :: (Integral a) => a -> [FizzBuzz]
fizzBuzz x = toFizzBuzz 3 x Fizz `mappend` toFizzBuzz 5 x Buzz

fizzBuzzSay :: String -> String
fizzBuzzSay s = showFizzBuzz ((readSome s :: [Int]) >>= fizzBuzz) s
    where
        showFizzBuzz [] s = s
        showFizzBuzz rs s = concat $ map show rs


toFizzBuzz
    :: (Integral a) => a -> a  -- Integral to test and modulo
    -> FizzBuzz -> [FizzBuzz]  -- Output FizzBuzz, wrapped within []
toFizzBuzz m x fb = do
    guard $ x >= m
    guard $ x `mod` m == 0
    return fb

readSome :: (Read a) => String -> [a]
readSome s = toSome (reads s)
    where
        toSome [(x, "")] = [x]
        toSome _ = []
