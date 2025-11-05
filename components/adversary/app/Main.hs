{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import Adversary (adversary, toString)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    msg <- adversary args
    putStrLn $ toString msg
