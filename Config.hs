{-# LANGUAGE DeriveDataTypeable #-}
module Config( Config
             , getConfig
             , configImages
             , configRows
             , configCols
             , configWidth
             , configHeight
             , configFS
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

data ConfigParameter = ConfigParameter { file       :: String
                                       , imagedir   :: Maybe String
                                       , rows       :: Maybe Int
                                       , cols       :: Maybe Int
                                       , width      :: Maybe Int
                                       , height     :: Maybe Int
                                       , fullscreen :: Maybe Bool
                                       , shuffled   :: Maybe Bool
                                       } deriving (Show, Data, Typeable)

configParameter :: ConfigParameter
configParameter = ConfigParameter{ file       = "recogs.cnf"
                                 , imagedir   = Nothing
                                 , rows       = Nothing
                                 , cols       = Nothing
                                 , width      = Nothing
                                 , height     = Nothing
                                 , fullscreen = Nothing
                                 , shuffled   = Nothing
                                 }

data Config = Config {configImages :: [String]
                     ,configRows   :: Int
                     ,configCols   :: Int
                     ,configWidth  :: Int
                     ,configHeight :: Int
                     ,configFS     :: Bool
                     } deriving (Show)

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

getInt :: String -> Maybe Int -> Either ParseError ConfigMap -> Maybe Int
getInt fieldName field (Right confFromFile) =
        case field of
          Just n  -> Just n
          Nothing -> fmap read (Map.lookup fieldName confFromFile)
getInt _ _ (Left _) = Nothing

getBool :: String -> Maybe Bool -> Either ParseError ConfigMap -> Maybe Bool
getBool fieldName field (Right confFromFile) =
        case field of
          Just n  -> Just n
          Nothing -> fmap read (Map.lookup fieldName confFromFile)
getBool _ _ (Left _) = Nothing

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
    let r' = fromMaybe 3 $ getInt "rows" (rows c) confFromFile
        c' = fromMaybe 4 $ getInt "cols" (cols c) confFromFile
        w' = fromMaybe 800 $ getInt "width" (width c) confFromFile
        h' = fromMaybe 600 $ getInt "height" (height c) confFromFile
        s' = fromMaybe False $ getBool "shuffled" (shuffled c) confFromFile
        f' = fromMaybe False $ getBool "fullscreen" (fullscreen c) confFromFile
        d' = getImageDir c confFromFile
    imgDir <- getImages d' s'
    let imageFiles = filter imageFile imgDir
    return Config { configImages = imageFiles
                  , configRows   = r'
                  , configCols   = c'
                  , configWidth  = w'
                  , configHeight = h'
                  , configFS     = f'
                  }
