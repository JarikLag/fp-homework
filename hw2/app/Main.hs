module Main 
  ( main
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad.State (runState)
import Control.Monad.Except (runExceptT)
import System.Exit (exitFailure)
import System.IO (stdout, hFlush)
import Types 
  ( Command (Exit, Help)

  , RootDirectory
  , FSState
  )
import Parser (parseCommand)
import FileManager 
  ( buildFileSystem
  , performCommand
  , saveFileSystemOnDisk

  , getPrefix
  , printHelp
  )

main :: IO ()
main = do
  putStrLn "Enter valid path to directory which must be root."
  rootPath <- getLine
  fs <- catch (buildFileSystem rootPath)
    (\e -> do 
      let err = show (e :: SomeException)
      putStrLn ("Exception occured while building FS: " ++ err)
      putStrLn ("Shutting down.")
      exitFailure)
  putStrLn "FileManager is ready. To see the list of available commands type: help"
  nFs <- commandLine (fs, fs)
  saveFileSystemOnDisk fs nFs

commandLine :: FSState -> IO RootDirectory
commandLine state@(rd, _) = do
  putStr $ getPrefix state
  hFlush stdout
  str <- getLine
  let maybeCmd = parseCommand str
  case maybeCmd of
    Just cmd ->
      case cmd of
        Help -> do
          printHelp
          commandLine state
        Exit -> return rd
        _    -> do
          let (res, nState) = runState (runExceptT $ performCommand cmd) state
          case res of
            Left except -> do
              putStrLn $ show except
              commandLine nState
            Right info -> do
              putStr info
              commandLine nState
    Nothing -> do
      putStrLn "Failed to parse command. Type 'help' command to see usage."
      commandLine state