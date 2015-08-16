module Hibernate where

import Control.Monad
import qualified System.Directory as Dir
import System.FilePath ((</>), makeRelative, takeDirectory)
import Control.Arrow
import System.IO
import Data.List
import Table
import qualified Data.Map as Map
import Debug.Trace

collectFiles :: FilePath -> IO [(Int, FilePath)]
collectFiles path = mapM pairWithSize =<< collectFileNames path

collectFileNames :: FilePath -> IO [FilePath]
collectFileNames path = do
    cont  <- map (path </>) <$> filter (`notElem` [".", ".."]) <$> Dir.getDirectoryContents path
    files <- filterM Dir.doesFileExist cont
    dirs  <- filterM Dir.doesDirectoryExist cont
    rest  <- concat <$> mapM collectFileNames dirs
    return $ files ++ rest

pairWithSize :: FilePath -> IO (Int, FilePath)
pairWithSize path = do
    size <- withFile path ReadMode hFileSize
    return (fromIntegral size, path)

takeOneMore :: (a -> Bool) -> [a] -> [a]
takeOneMore f l = left ++ [head right]
    where (left, right) = partition f l

getTop90 :: [(Int, FilePath)] -> [FilePath]
getTop90 list = map snd . takeOneMore ((<= target) . fst) $ sums
    where sums = scanl1 (\(s1, p1) (s2, p2) -> (s1 + s2, p2)) . sortBy (flip compare) $ list
          total = fst . last $ sums
          target = (total * 9) `div` 10

selectFiles :: FilePath -> IO [FilePath]
selectFiles path = getTop90 <$> (collectFiles =<< Dir.makeAbsolute path)

copy :: FilePath -> FilePath -> IO ()
copy old new = do
    putStrLn $ "Copying... " ++ old ++ " " ++ new
    Dir.createDirectoryIfMissing True $ takeDirectory new
    Dir.copyFile old new

hibernate :: FilePath -> IO ()
hibernate relPath = do
    path <- Dir.makeAbsolute relPath
    (c, m) <- loadTable
    if Map.member path m then putStrLn "Folder already hibernated"
    else do tgt <- targetPath <$> loadConfig
            alreadyExists <- Dir.doesDirectoryExist (tgt </> show c)
            when alreadyExists $ Dir.removeDirectoryRecursive (tgt </> show c)
            files <- selectFiles path
            zipWithM_ copy files $ map (((tgt </> show c) </>) . makeRelative path) files
            mapM_ Dir.removeFile files
            saveTable (c + 1, Map.insert path (show c) m)

unhibernate :: FilePath -> IO ()
unhibernate relPath = do
    path <- Dir.makeAbsolute relPath
    (c, m) <- loadTable
    print path
    if Map.notMember path m then putStrLn "Folder not hibernated"
    else do tgt <- targetPath <$> loadConfig
            let tgtDir = tgt </> (m Map.! path)
            files <- collectFileNames tgtDir
            zipWithM_ copy files $ map ((path </>) . makeRelative tgtDir) files
            Dir.removeDirectoryRecursive tgtDir
            saveTable (c, Map.delete path m)
