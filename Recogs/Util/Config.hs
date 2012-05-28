module Recogs.Util.Config( getConfig ) where
import Data.Char
import Data.List
import qualified Data.Map as Map
import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Maybe
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import Recogs.Data (Config(..), ConfigParameter(..), ConfigMap)
import Recogs.Util.RandomList (shuffle)

configParameter :: ConfigParameter
configParameter = ConfigParameter
                  { cpFile       = "recogs.cnf" &= explicit             &= name "file"       &= help "configuration file"          &= groupname "Set-Up" &= typFile
                  , cpImagedir   = Nothing      &= explicit &= name "d" &= name "imagedir"   &= help "directory containing images" &= groupname "Images" &= typDir 
                  , cpShuffled   = Nothing      &= explicit &= name "s" &= name "shuffled"   &= help "randomize order of images"                                   
                  , cpRows       = Nothing      &= explicit &= name "r" &= name "rows"       &= help "#segments per col"           &= groupname "Level"            
                  , cpCols       = Nothing      &= explicit &= name "c" &= name "cols"       &= help "#segments per row"                                           
                  , cpWidth      = Nothing      &= explicit &= name "w" &= name "width"      &= help "width of program window"     &= groupname "Display"          
                  , cpHeight     = Nothing      &= explicit &= name "h" &= name "height"     &= help "height of program window"                                    
                  , cpFullscreen = Nothing      &= explicit &= name "f" &= name "fullscreen" &= help "fullscreen overrides width/height"                           
                  } &= program "recogs"

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
        case cpImagedir confMap of
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
    confFromFile <- readConfig $ cpFile c
    let r' = fromMaybe 3 $ getInt "rows" (cpRows c) confFromFile
        c' = fromMaybe 4 $ getInt "cols" (cpCols c) confFromFile
        w' = fromMaybe 800 $ getInt "width" (cpWidth c) confFromFile
        h' = fromMaybe 600 $ getInt "height" (cpHeight c) confFromFile
        s' = fromMaybe False $ getBool "shuffled" (cpShuffled c) confFromFile
        f' = fromMaybe False $ getBool "fullscreen" (cpFullscreen c) confFromFile
        d' = getImageDir c confFromFile
    imgDir <- getImages d' s'
    let imageFiles = "recogs.png" : filter imageFile imgDir
    return Config { configImages = imageFiles
                  , configRows   = r'
                  , configCols   = c'
                  , configWidth  = w'
                  , configHeight = h'
                  , configFS     = f'
                  }
