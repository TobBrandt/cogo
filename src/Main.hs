module Main where

import Cogo

import System.Environment
import System.Exit
import System.IO

import Text.Printf

defaultBoardSize :: Double
defaultBoardSize = 10

main :: IO ()
main = 
    do
        args <- getArgs
        case parseArgs args of
            Nothing -> usageAndExit
            Just conf -> cogoMain conf

usageAndExit :: IO ()
usageAndExit =
    do
        progname <- getProgName
        -- TODO add options
        hPrintf stderr "usage: %s\n" progname
        exitFailure

parseArgs :: [String] -> Maybe Config
parseArgs args = Just $ Config 
  -- TODO
  { configBoardSize = defaultBoardSize
  }
