module Main where

sample x = x + x

main :: IO ()
main = putStrLn $ show $ sample 2
