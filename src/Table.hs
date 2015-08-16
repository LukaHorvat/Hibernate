module Table where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified System.Directory as Dir
import System.FilePath ((</>))

loadTable :: IO (Int, Map String String)
loadTable = loadFromAppDir "table"

saveTable :: (Int, Map String String) -> IO ()
saveTable = saveToAppDir "table"

data Config = Config { exePath    :: FilePath
                     , targetPath :: FilePath } deriving (Eq, Ord, Show, Read)

loadConfig :: IO Config
loadConfig = loadFromAppDir "config"

saveConfig :: Config -> IO ()
saveConfig = saveToAppDir "config"

loadFromAppDir :: Read a => String -> IO a
loadFromAppDir file = do
    appDir <- Dir.getAppUserDataDirectory "Hibernate001"
    read <$> readFile (appDir </> file)

saveToAppDir :: Show a => String -> a -> IO ()
saveToAppDir file value = do
    appDir <- Dir.getAppUserDataDirectory "Hibernate001"
    writeFile (appDir </> file) (show value)
