import Control.Applicative (Alternative ((<|>)))
import Control.Monad (liftM)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, isJust)
import System.Directory (doesFileExist)
import System.Environment (getArgs, lookupEnv)

type Query = String

type FileContents = String

data Config = Config {query :: Query, filePath :: FilePath, caseSensitive :: Bool}

lowerCase :: String -> String
lowerCase = map toLower

isCaseSensitive :: IO Bool
isCaseSensitive = do
  envVariable <- lookupEnv "CASE_SENSITIVE"
  let isEnvDeclared = isJust envVariable
  case envVariable of
    Nothing -> return False
    Just caseSensitive -> return $ lowerCase caseSensitive == "true"

type Args = [String]

getQuery :: Args -> Maybe Query
getQuery args =
  if null args
    then Nothing
    else Just $ head args

getFilePath :: Args -> Maybe FilePath
getFilePath args =
  if length args < 2
    then Nothing
    else Just $ args !! 1

getConfig :: MaybeT IO Config
getConfig =
  do
    args <- MaybeT $ Just <$> getArgs

    query <- (MaybeT . return . getQuery $ args) <|> MaybeT (Nothing <$ putStrLn "Didn't get a query!")
    filePath <- (MaybeT . return . getFilePath $ args) <|> MaybeT (Nothing <$ putStrLn "Didn't get a file path!")

    caseSensitive <- MaybeT $ Just <$> isCaseSensitive
    return $ Config {query = query, filePath = filePath, caseSensitive = caseSensitive}

type SearchMethod = Query -> String -> Bool

search :: SearchMethod -> Query -> FileContents -> [String]
search method query fileContents = filter (method query) $ lines fileContents

caseSensitiveSearch :: SearchMethod
caseSensitiveSearch = isInfixOf

caseInsensitiveSearch :: SearchMethod
caseInsensitiveSearch = applyToBothArguments lowerCase isInfixOf
  where
    applyToBothArguments :: (a -> b) -> (b -> b -> c) -> a -> a -> c
    applyToBothArguments f g x y = g (f x) (f y)

getSearchMethod :: Bool -> SearchMethod
getSearchMethod caseSensitive =
  if caseSensitive
    then caseSensitiveSearch
    else caseInsensitiveSearch

run :: Config -> IO ()
run Config {query = query, filePath = filePath, caseSensitive = caseSensitive} =
  do
    fileExists <- doesFileExist filePath
    if not fileExists
      then putStrLn "File doesn't exist!"
      else do
        fileContents <- readFile filePath
        let searchMethod = getSearchMethod caseSensitive
            matches = search searchMethod query fileContents
        putStr $ unlines matches

main :: IO ()
main = do
  config <- runMaybeT getConfig
  mapM_ run config