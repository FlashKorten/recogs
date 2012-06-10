module Recogs.Util.Config( getConfig ) where

import Data.Char ( isSpace, toLower )
import Data.List ( sort )
import qualified Data.Map as Map ( lookup, fromList )
import Control.Monad ( liftM, void, msum )
import Data.Maybe ( catMaybes, fromJust)
import Text.ParserCombinators.Parsec
    ( Parser
    , SourceName
    , try
    , parseFromFile
    , skipMany
    , many
    , (<|>)
    , (<?>)
    , manyTill
    , eof
    , space
    , oneOf
    , noneOf
    , letter
    , digit
    , char
    , anyChar
    )
import System.Console.CmdArgs
    ( typFile
    , typDir
    , program
    , name
    , help
    , groupname
    , explicit
    , cmdArgs
    , (&=)
    )
import System.Directory ( getDirectoryContents )
import System.FilePath ( takeExtension, (</>) )
import Recogs.Data ( Config(..), ConfigParameter(..), ConfigMap )
import Recogs.Util.RandomList ( shuffle )

configParameter :: ConfigParameter
configParameter = ConfigParameter
                  { cpFile       = "recogs.cnf" &= explicit             &= name "file"
                                                &= help "configuration file"          &= groupname "Set-Up" &= typFile
                  , cpImagedir   = Nothing      &= explicit &= name "d" &= name "imagedir"
                                                &= help "directory containing images" &= groupname "Images" &= typDir
                  , cpShuffled   = Nothing      &= explicit &= name "s" &= name "shuffled"
                                                &= help "randomize order of images"
                  , cpRows       = Nothing      &= explicit &= name "r" &= name "rows"
                                                &= help "#segments per col"           &= groupname "Level"
                  , cpCols       = Nothing      &= explicit &= name "c" &= name "cols"
                                                &= help "#segments per row"
                  , cpWidth      = Nothing      &= explicit &= name "w" &= name "width"
                                                &= help "width of program window"     &= groupname "Display"
                  , cpHeight     = Nothing      &= explicit &= name "h" &= name "height"
                                                &= help "height of program window"
                  , cpFullscreen = Nothing      &= explicit &= name "f" &= name "fullscreen"
                                                &= help "fullscreen overrides width/height"
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
eol = void (oneOf "\n\r") <?> "end of line"

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

readConfig :: SourceName -> IO (Maybe ConfigMap)
readConfig fileName = do
    conf <- liftM (liftM Map.fromList) (parseFromFile fileContent fileName)
    case conf of
      Left err -> print err >> return Nothing
      Right cm -> return $ Just cm

getParameter :: (Read a) => String -> Maybe a -> Maybe ConfigMap -> a -> a
getParameter fieldName field confFile fallback =
    fromJust $ msum [ field
                    , fmap read (confFile >>= Map.lookup fieldName)
                    , Just fallback
                    ]

getImages :: FilePath -> Bool -> IO [FilePath]
getImages filePath random
    | random    = liftM (map (filePath </>))       $ shuffle =<< getDirectoryContents filePath
    | otherwise = liftM (map (filePath </>). sort) $ getDirectoryContents filePath

imageFile :: FilePath -> Bool
imageFile filePath = fileType `elem` [".jpg", ".jpeg", ".png"]
                  where fileType = map toLower $ takeExtension filePath

getConfig :: IO Config
getConfig = do
    c <- cmdArgs configParameter
    confFromFile <- readConfig $ cpFile c
    let r' = getParameter "rows"       (cpRows c)       confFromFile 3
        c' = getParameter "cols"       (cpCols c)       confFromFile 4
        w' = getParameter "width"      (cpWidth c)      confFromFile 800
        h' = getParameter "height"     (cpHeight c)     confFromFile 800
        s' = getParameter "shuffled"   (cpShuffled c)   confFromFile False
        f' = getParameter "fullscreen" (cpFullscreen c) confFromFile False
        d' = getParameter "imagedir"   (cpImagedir c)   confFromFile "."
    imgDir <- getImages d' s'
    let imageFiles = "recogs.png" : filter imageFile imgDir
    return Config { configImages = imageFiles
                  , configRows   = r'
                  , configCols   = c'
                  , configWidth  = w'
                  , configHeight = h'
                  , configFS     = f'
                  }
