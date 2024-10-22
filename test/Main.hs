module Main (main) where

import qualified Core.Algebra as CA

assert :: Bool -> String -> IO ()
assert True msg = putStrLn msg
assert _ msg = error ("Test failed: " ++ msg)

assertEq :: Eq a => a -> a -> String -> IO ()
assertEq l r msg = assert (l == r) msg

mergeSuit :: IO ()
mergeSuit = do assertEq (CA.merge [1::Int] []) [1] "merge [1] [] == [1]"
               assertEq (CA.merge [] [1::Int]) [1] "merge [] [1] == [1]"
               assertEq (CA.merge [1::Int,2,3] [3,4,5]) [1,2,3,3,4,5] "merge [1,2,3] [3,4,5] == [1,2,3,3,4,5]"

sortSuit :: IO ()
sortSuit = do assertEq (CA.sort [1::Int]) [1] "sort [1] == [1]"
              assertEq (CA.sort [5::Int,1,2,4,3]) [1,2,3,4,5] "sort [5,1,2,4,3] == [1,2,3,4,5]"
              assertEq (CA.sort [3::Int,1,3]) [1,3,3] "sort [3,1,3] == [1,3,3]"

main :: IO ()
main = do mergeSuit
          sortSuit
