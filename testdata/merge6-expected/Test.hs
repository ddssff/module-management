module Main where

import A (a, b)
import B.D (d)

main = putStrLn $ "Hello, world. a=" ++ show a ++ ", b=" ++ show b ++ ", d=" ++ show d
