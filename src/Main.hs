module Main where

import Parser
import Typing.Typechecker
import System.IO

process :: String -> String
process = show . typecheck . parse "<interpreted>"

main::IO()
main = do
        hSetBuffering stdout NoBuffering
        interact $ unlines . map process . lines
