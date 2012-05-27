{-# LANGUAGE DeriveDataTypeable #-}
module Config(
              Config,
              getConfig,
              configImages,
              configRows,
              configCols
             ) where
import Data.Char
import Data.List
import Control.Monad
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Data.Maybe
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import RandomList (shuffle)

data ConfigParameter = ConfigParameter { file     :: String
                                       , imagedir :: Maybe String
                                       , rows     :: Maybe Int
                                       , cols     :: Maybe Int
                                       , shuffled :: Maybe Bool
                                       }
              deriving (Show, Data, Typeable)

configParameter :: ConfigParameter
configParameter = ConfigParameter{ file     = "recogs.cnf"
                                 , imagedir = Nothing
                                 , rows     = Nothing
                                 , cols     = Nothing
                                 , shuffled = Nothing
                                 }

data Config = Config {configImages :: [String]
                     ,configRows   :: Int
                     ,configCols   :: Int
                     }
              deriving (Show)

type ConfigMap = Map.Map String String

ident :: Parser String
ident = do c <- letter <|> char '_'
           cs <- many (letter <|> digit <|> char '_')
           return (c:cs)
      <?> "identifier"

comment :: Parser ()
comment = do _ <- char '#'
             skipMany (noneOf "\r\n")
        <?> "comment"

eol :: Parser ()
eol = do _ <- oneOf "\n\r"
         return ()
    <?> "end of line"

item :: Parser (String, String)
item = do key <- ident
          skipMany space
          _ <- char '='
          skipMany space
          value <- manyTill anyChar (try eol <|> try comment <|> eof)
          return (key, rstrip value)
    where rstrip = reverse . dropWhile isSpace . reverse

line :: Parser (Maybe (String, String))
line = do skipMany space
          try (comment >> return Nothing) <|> liftM Just item

fileContent :: Parser [(String, String)]
fileContent = do allLines <- many line
                 return (catMaybes allLines)

readConfig :: SourceName -> IO (Either ParseError ConfigMap)
readConfig fileName =
    liftM (liftM Map.fromList) (parseFromFile fileContent fileName)

getInt :: String -> (ConfigParameter -> Maybe Int) -> ConfigParameter -> Either ParseError ConfigMap -> Maybe Int
getInt fieldName f confMap (Right confFromFile) =
        case f confMap of
          Just n  -> Just n
          Nothing -> fmap read (Map.lookup fieldName confFromFile)
getInt _ _ _ (Left _) = Nothing

getShuffled :: ConfigParameter -> Either ParseError ConfigMap -> Maybe Bool
getShuffled confMap (Right confFromFile) =
        case shuffled confMap of
          Just n  -> Just n
          Nothing -> fmap read (Map.lookup "shuffled" confFromFile)
getShuffled _ (Left _) = Nothing

getImageDir :: ConfigParameter -> Either ParseError ConfigMap -> Maybe FilePath
getImageDir confMap (Right confFromFile) =
        case imagedir confMap of
          Just n  -> Just n
          Nothing -> fmap read (Map.lookup "imagedir" confFromFile)
getImageDir _ (Left _) = Nothing

getImages :: Maybe FilePath -> Bool -> IO [FilePath]
getImages Nothing         _      = return []
getImages (Just filePath) random
    | random    = liftM (map (filePath </>))       $ shuffle =<< getDirectoryContents filePath
    | otherwise = liftM (map (filePath </>). sort) $ getDirectoryContents filePath

imageFile :: FilePath -> Bool
imageFile filePath = fileType `elem` [".jpg", ".jpeg", ".png"]
                  where fileType = map toLower $ takeExtension filePath

getConfig :: IO Config
getConfig = do
    c <- cmdArgs configParameter
    confFromFile <- readConfig $ file c
    let r' = getInt "rows" rows c confFromFile
        c' = getInt "cols" cols c confFromFile
        d' = getImageDir c confFromFile
        s' = fromMaybe False $ getShuffled c confFromFile
    imgDir <- getImages d' s'
    let imageFiles = filter imageFile imgDir
    return Config{configImages=imageFiles, configRows = fromMaybe 3 r', configCols = fromMaybe 4 c'}
