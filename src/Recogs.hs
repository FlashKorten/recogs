{-# LANGUAGE RankNTypes #-}
module Main where

import Graphics.UI.SDL as SDL
    ( Surface
    , SurfaceFlag(Fullscreen)
    , Rect(Rect)
    , SDLKey(SDLK_BACKSPACE, SDLK_ESCAPE, SDLK_RETURN, SDLK_SPACE, SDLK_m)
    , Keysym(Keysym)
    , InitFlag(InitEverything)
    , Event(KeyUp)
    , Pixel(Pixel)
    , setCaption
    , setVideoMode
    , getVideoSurface
    , getVideoInfo
    , flip
    , fillRect
    , blitSurface
    , videoInfoWidth
    , videoInfoHeight
    , surfaceGetWidth
    , surfaceGetHeight
    , freeSurface
    , quit
    , init
    , waitEventBlocking
    )
import Graphics.UI.SDL.Image ( load )
import Graphics.UI.SDL.Rotozoomer ( zoom )
import Control.Monad ( unless )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import System.Exit ( exitSuccess )
import Recogs.Util.Config ( getConfig )
import Recogs.Util.RandomList ( shuffle )
import Recogs.Data ( Game(..)
                   , Dimension
                   , Coord
                   , Config( configCols
                           , configFS
                           , configHeight
                           , configImages
                           , configRows
                           , configWidth)
                   )

main :: IO ()
main = do
    SDL.init [InitEverything]
    conf <- getConfig
    config <- createAdjustedWindow "Recogs" conf
    game <- initGame config
    gameRef <- newIORef game
    display gameRef
    eventLoop gameRef

eventLoop :: IORef Game -> IO ()
eventLoop game = SDL.waitEventBlocking >>= handleEvent game

handleEvent :: IORef Game -> Event -> IO ()
handleEvent _    (SDL.KeyUp (Keysym SDLK_ESCAPE _ _))    = exit
handleEvent game (SDL.KeyUp (Keysym k _ _))              = handleKey game k >> eventLoop game
handleEvent game _                                       = eventLoop game

handleKey :: IORef Game -> SDLKey -> IO ()
handleKey game SDLK_SPACE     = doNextStep game (subtract 1)
handleKey game SDLK_BACKSPACE = doNextStep game (+1)
handleKey game SDLK_RETURN    = revealOrStartNext game
handleKey game SDLK_m         = reshuffle game
handleKey _ _                 = return ()

getSurfaceDimension :: Surface -> Dimension
getSurfaceDimension s = (surfaceGetWidth s, surfaceGetHeight s)

getScaleFactor :: Dimension -> Dimension -> Double
getScaleFactor (w1, h1) (w2, h2) = min (divide w2 w1) (divide h2 h1)
        where divide a b = fromIntegral a / fromIntegral b

getOffset :: Dimension -> Dimension -> Dimension
getOffset (w1, h1) (w2, h2) = (offset w1 w2, offset h1 h2)
        where offset :: Int -> Int -> Int
              offset x y | x == y    = 0
                         | otherwise = abs $ (x - y) `div` 2

getOffsetRect :: Dimension -> Dimension -> Maybe Rect
getOffsetRect a b
    | a == b    = Nothing
    | otherwise = Just $ Rect x y 0 0
                    where (x, y) = getOffset a b

rangeCheck :: Ord a => a -> a -> a -> a
rangeCheck lowerBound upperBound n
    | n < lowerBound = lowerBound
    | n > upperBound = upperBound
    | otherwise      = n

maxSteps :: Config -> Int
maxSteps c = configRows c * configCols c

coordinates :: Int -> Int -> [Coord]
coordinates rows cols = [(r, c)| r <- [0..(rows - 1)], c <- [0..(cols - 1)]]

scaleImage :: Surface -> Double -> IO Surface
scaleImage s d = zoom s d d True

calculateBlockDimension :: Dimension -> Int -> Int -> Dimension
calculateBlockDimension (w, h) r c = ((w `div` c) + 1, (h `div` r) + 1)

calculateImageData :: Surface -> Config -> IO (Surface, [Rect], Dimension)
calculateImageData image config = do
    screen <- getVideoSurface
    let sf              = getScaleFactor imageDimension screenDimension
        imageDimension  = getSurfaceDimension image
        screenDimension = getSurfaceDimension screen
    scaledImage <- scaleImage image sf
    let offset    = getOffset scaledImageDimension screenDimension
        scaledImageDimension = getSurfaceDimension scaledImage
    rects <- shuffle $ calculateRects scaledImageDimension offset (configRows config, configCols config)
    return (scaledImage, rects, offset)

initImageData :: Config -> IO (Surface, [Rect], Dimension)
initImageData config = do
    image  <- load $ head $ configImages config
    calculateImageData image config

updateImageData :: IORef Game -> IO ()
updateImageData gameRef = do
    g <- readIORef gameRef
    freeSurface $ getImage g
    let c = getConf g
    image  <- load $ configImages c !! getFileNr g
    (scaledImage, rects, offset) <- calculateImageData image c
    writeIORef gameRef g{getImage = scaledImage, getRects = rects, getBaseOffset = offset}

initGame :: Config -> IO Game
initGame config = do
    (image, rects, offset) <- initImageData config
    return Game { getConf           = config
                , getRects          = rects
                , getStep           = 0
                , getImage          = image
                , getBaseOffset     = offset
                , getFileNr         = 0
                }

doNextStep' :: Game -> (Int -> Int) -> (Game, IsUnchanged)
doNextStep' game f = if lastStep == nextStep
                        then (game, True)
                        else (game {getStep = nextStep}, False)
                      where lastStep   = getStep game
                            nextStep   = rangeCheck 0 upperBound $ f lastStep
                            upperBound = maxSteps $ getConf game

doNextStep :: IORef Game -> (Int -> Int) -> IO ()
doNextStep gameRef f = do
    game <- readIORef gameRef
    let (newState, unchanged) = doNextStep' game f
    unless unchanged $ do
        writeIORef gameRef newState
        display gameRef

type IsUnchanged = Bool
nextRound :: IORef Game -> IO ()
nextRound gameRef = do
    g <- readIORef gameRef
    let fileNr    = getFileNr g
        config    = getConf g
        imgFiles  = configImages config
    if fileNr == length imgFiles - 1
      then exitSuccess
      else do
        writeIORef gameRef g{ getFileNr  = fileNr + 1
                            , getStep    = (configRows config) * (configCols config)
                            }
        updateImageData gameRef
        clearScreen

reshuffle :: IORef Game -> IO ()
reshuffle gameRef = do
    g <- readIORef gameRef
    rects <- shuffle $ calculateRectsFromGame g
    writeIORef gameRef g{getRects = rects}
    display gameRef

revealOrStartNext :: IORef Game -> IO ()
revealOrStartNext gameRef = do
    g <- readIORef gameRef
    if getStep g == 0
      then nextRound gameRef
      else doNextStep gameRef (*0)

clearScreen :: IO ()
clearScreen = do
    screen <- getVideoSurface
    _ <- fillRect screen (Just (Rect 0 0 (surfaceGetWidth screen) (surfaceGetHeight screen))) (Pixel 0x000000)
    SDL.flip screen

display :: IORef Game -> IO ()
display gameRef = do
    g <- readIORef gameRef
    screen <- getVideoSurface
    _ <- showImage (getImage g) screen
    mapM_ (drawRect screen) $ take (getStep g) (getRects g)
    SDL.flip screen

showImage :: Surface -> Surface -> IO Bool
showImage surface screen = blitSurface surface Nothing screen $ getOffsetRect (getSurfaceDimension surface) (getSurfaceDimension screen)

drawRect :: Surface -> Rect -> IO ()
drawRect s r = do
    _ <- fillRect s (Just r) (Pixel 0x000000)
    return ()

calculateRectsFromGame :: Game -> [Rect]
calculateRectsFromGame g = calculateRects imageDimension offset (rows, cols)
                             where imageDimension = getSurfaceDimension $ getImage g
                                   offset         = getBaseOffset g
                                   rows           = configRows config
                                   cols           = configCols config
                                   config         = getConf g

calculateRects :: Dimension -> Dimension -> Dimension -> [Rect]
calculateRects (wImage, hImage) offset (rows, cols) = map (calculateRect (width, height) offset) coords
                                                        where width  = wImage `div` cols + 1
                                                              height = hImage `div` rows + 1
                                                              coords = [(r, c)| r <- [0..(rows - 1)], c <- [0..(cols - 1)]]

calculateRect :: Dimension -> Dimension -> Coord -> Rect
calculateRect (width, height) (xOffset, yOffset) (row, col) = Rect x y width height
                                                                where x = (col * width) + xOffset
                                                                      y = (row * height) + yOffset

createAdjustedWindow :: String -> Config -> IO Config
createAdjustedWindow title c
    | configFS c = do
                   info <- getVideoInfo
                   let w = videoInfoWidth info
                       h = videoInfoHeight info
                   _ <- setVideoMode w h 32 [Fullscreen]
                   return c{ configWidth  = w
                           , configHeight = h
                           }
    | otherwise  = do
                   _ <- setVideoMode (fromIntegral $ configWidth c) (fromIntegral $ configHeight c) 32 []
                   _ <- SDL.setCaption title title
                   return c

exit :: IO ()
exit = do
    SDL.quit
    print "done..."

