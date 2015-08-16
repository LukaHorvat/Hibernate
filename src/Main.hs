module Main where

import qualified System.Directory as Dir
import qualified System.IO as IO
import qualified System.Win32 as Win
import qualified System.Environment as Env
import qualified System.Process as Proc
import qualified System.FilePath as Path
import System.FilePath ((</>))
import Control.Applicative
import Control.Monad
import Data.Char
import Hibernate
import Table
import qualified Data.Map as Map
import Control.Exception

{-
Check if installed
Yes
    Load config
    Check if this instance is the main one
    Yes
        Check if there are arguments
        Yes
            Hibernate/unhibernate folder
        No
            Ask to add/remove registry key
    No
        Load table data
        Restore current hibernated folder
No
    Ask is exe placed in desired location
    Ask for hibernation target location
    Create configuraion and table data
    Add registry key
-}

main :: IO ()
main = do
    appDir <- Dir.getAppUserDataDirectory "Hibernate001"
    installed <- Dir.doesDirectoryExist appDir
    if installed then run
    else install

prompt :: String -> IO String
prompt text = do
    putStrLn text
    getLine

install :: IO ()
install = do
    confirm <- prompt "Are you sure this the current location of this file is where you want it installed? (y/n)"
    case map toLower confirm of
        'y' : _ -> do
            appDir <- Dir.getAppUserDataDirectory "Hibernate001"
            Dir.createDirectoryIfMissing True appDir
            hibernationFolder <- prompt "Please specify a path where hibernated folders will be placed"
            thisPath <- Env.getExecutablePath
            saveConfig (Config thisPath hibernationFolder)
            saveTable (0, Map.empty)
            addRegistryEntry
        _ -> return ()

run :: IO ()
run = do
    args <- Env.getArgs
    if null args then do
        addRemove <- map toLower
                 <$> prompt "Do you want to [a]dd or [r]emove the program from Explorer's context menu?"
        case addRemove of
            'a':_ -> addRegistryEntry
            'r':_ -> removeRegistryEntry
            _     -> do putStrLn "Unrecognized option. Closing on key press..."
                        void getChar
    else do putStrLn "Running..."
            handle (\e -> print (e :: IOException)) $ case args of
                "hibernate" : dir   -> hibernate $ unwords dir
                "unhibernate" : dir -> unhibernate $ unwords dir
                opts                -> putStrLn "Unrecognized command line options" >> print opts
            putStrLn "Done!"
            void getChar

addRegistryEntry :: IO ()
addRegistryEntry = do
    thisPath <- Env.getExecutablePath
    let hibCmd   = "\"\\\"" ++ thisPath ++ "\\\" hibernate %%1\""
        unhibCmd = "\"\\\"" ++ thisPath ++ "\\\" unhibernate %%1\""
        code = unlines
            [ "REG ADD HKCR\\Folder\\shell\\Hibernate\\command /f /d "   ++ hibCmd
            , "REG ADD HKCR\\Folder\\shell\\Unhibernate\\command /f /d " ++ unhibCmd ]
    runAsAdmin code

removeRegistryEntry :: IO ()
removeRegistryEntry = do
    thisPath <- Env.getExecutablePath
    let code = unlines
            [ "REG DELETE HKCR\\Folder\\shell\\Hibernate /f"
            , "REG DELETE HKCR\\Folder\\shell\\Unhibernate /f" ]
    runAsAdmin code

runAsAdmin :: String -> IO ()
runAsAdmin code = do
    writeFile "reg.bat" code
    void $ Proc.runCommand "powershell \"saps -filepath reg.bat -verb runas -wait\"" >>= Proc.waitForProcess
    Dir.removeFile "reg.bat"
