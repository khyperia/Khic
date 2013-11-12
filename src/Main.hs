module Main where

import System.IO
import System.Process
import System.Directory
import Parser
import Typing.Typechecker
import BackendLLVM.Emit
import BackendLLVM.Compile

link :: IO ()
link = do
        handle <- runCommand "ld a.obj -lmsvcrt"
        _ <- waitForProcess handle
        removeFile "a.obj"
        return ()

process :: String -> IO ()
process = compile . emit . typecheck . parse "<interpreted>"

main::IO()
main = do
        hSetBuffering stdout NoBuffering
        content <- readFile "testProgram.kc"
        () <- process content
        putStrLn "linking"
        () <- link
        putStrLn "done"
