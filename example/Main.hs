{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck ( quickCheck )
import Test.Filter ( sweep, mark )

prop1 :: [Int] -> Bool
prop1 xs = xs == reverse (reverse xs)

prop2 :: Int -> Int -> Bool
prop2 x y = x + y == y + x

-- the number indicates which actions to run
#ifdef GROUP
$(mark GROUP)
#else
$(mark [0]) -- default
#endif

main :: IO ()
main = do
    _ <- sweep 1 $ quickCheck prop1 -- doesn't run
    _ <- sweep 0 $ quickCheck prop2 -- runs
    return ()



