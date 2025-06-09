module Main where

import System.Directory
import System.IO
import System.FilePath
import Control.Monad
import Data.List
import System.Process (callCommand)

main :: IO ()
main = do
    putStrLn "Welcome to Grenada"
    loop "."

loop :: FilePath -> IO ()
loop cwd = do
    putStr $ cwd ++ " > "
    hFlush stdout
    cmdLine <- getLine
    let cmdWords = words cmdLine
    case cmdWords of
        ("ls":_) -> do
            files <- listDirectory cwd
            mapM_ putStrLn files
            loop cwd
        ("cd":dir:_) -> do
            let newPath = cwd </> dir
            exists <- doesDirectoryExist newPath
            if exists
                then loop (normalise newPath)
                else putStrLn "Directory does not exist." >> loop cwd
        ("cat":file:_) -> do
            let filePath = cwd </> file
            exists <- doesFileExist filePath
            if exists
                then readFile filePath >>= putStrLn
                else putStrLn "File does not exist."
            loop cwd
        ("rm":file:_) -> do
            let filePath = cwd </> file
            exists <- doesFileExist filePath
            if exists
                then removeFile filePath >> putStrLn "File removed."
                else putStrLn "File does not exist."
            loop cwd
        ("mkdir":dir:_) -> do
            let dirPath = cwd </> dir
            createDirectory dirPath
            putStrLn "Directory created."
            loop cwd
        ("pwd":_) -> do
            putStrLn (normalise cwd)
            loop cwd
        ("clear":_) -> do
            callCommand "clear"
            loop cwd
        ("help":_) -> do
            putStrLn "Commands: ls, cd <dir>, cat <file>, rm <file>, mkdir <dir>, pwd, clear, help, exit"
            loop cwd
        ("exit":_) -> putStrLn "Exiting file manager."
        _ -> do
            putStrLn "Unknown command. Type 'help' for list of commands."
            loop cwd
