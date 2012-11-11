{-# LANGUAGE RankNTypes #-}
module Main where

import Graphics.UI.SDL as SDL
    ( Surface
    , SurfaceFlag(Fullscreen)
    , Rect(Rect)
    , SDLKey(SDLK_BACKSPACE, SDLK_ESCAPE, SDLK_RETURN, SDLK_SPACE, SDLK_m, SDLK_n)
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

showImage :: Surface -> Surface -> IO Bool
showImage surface screen = blitSurface surface Nothing screen $ getOffsetRect (getSurfaceDimension surface) (getSurfaceDimension screen)

maxSteps :: Config -> Int
maxSteps c = configRows c * configCols c

coordinates :: Int -> Int -> [Coord]
coordinates rows cols = [(r, c)| r <- [0..(rows - 1)], c <- [0..(cols - 1)]]

scaleImage :: Surface -> Double -> IO Surface
scaleImage s d = zoom s d d True

calculateBlockDimension :: Dimension -> Int -> Int -> Dimension
calculateBlockDimension (w, h) r c = ((w `div` c) + 1, (h `div` r) + 1)

calculateImageData :: Surface -> Config -> IO (Surface, Dimension, Dimension)
calculateImageData image config = do
    screen <- getVideoSurface
    let sf              = getScaleFactor imageDimension screenDimension
        imageDimension  = getSurfaceDimension image
        screenDimension = getSurfaceDimension screen
    scaledImage <- scaleImage image sf
    let offset    = getOffset scaledImageDimension screenDimension
        scaledImageDimension = getSurfaceDimension scaledImage
        blockDimension = calculateBlockDimension scaledImageDimension (configRows config) (configCols config)
    return (scaledImage, blockDimension, offset)

initImageData :: Config -> IO (Surface, Dimension, Dimension)
initImageData c = do
    image  <- load $ head $ configImages c
    calculateImageData image c

updateImageData :: IORef Game -> IO ()
updateImageData gameRef = do
    g <- readIORef gameRef
    freeSurface $ getImage g
    let c = getConf g
    image  <- load $ configImages c !! getFileNr g
    (scaledImage, blockDimension, offset) <- calculateImageData image c
    writeIORef gameRef g{getImage = scaledImage, getBlockDimension = blockDimension, getBaseOffset = offset}

initGame :: Config -> [Coord] -> IO Game
initGame config coords = do
    (image, blockDimension, offset) <- initImageData config
    return Game { getConf           = config
                , getCoords         = coords
                , getStep           = 0
                , getImage          = image
                , getBlockDimension = blockDimension
                , getBaseOffset     = offset
                , getFileNr         = 0
                }

nextRound :: IORef Game -> IO ()
nextRound gameRef = do
    g <- readIORef gameRef
    let fileNr    = getFileNr g
        config    = getConf g
        imgFiles  = configImages config
    if fileNr == length imgFiles - 1
      then exitSuccess
      else do
        let rows = configRows config
            cols = configCols config
        coords <- shuffle $ coordinates rows cols
        writeIORef gameRef g{ getCoords  = coords
                           , getFileNr  = fileNr + 1
                           , getStep    = length coords
                           }
        updateImageData gameRef
        clearScreen

clearScreen :: IO ()
clearScreen = do
    screen <- getVideoSurface
    _ <- fillRect screen (Just (Rect 0 0 (surfaceGetWidth screen) (surfaceGetHeight screen))) (Pixel 0x000000)
    SDL.flip screen

reshuffle :: IORef Game -> IO ()
reshuffle gameRef = do
    g <- readIORef gameRef
    let conf = getConf g
        rows = configRows conf
        cols = configCols conf
    c <- shuffle $ coordinates rows cols
    writeIORef gameRef g{getCoords = c}
    display gameRef

display :: IORef Game -> IO ()
display gameRef = do
    g <- readIORef gameRef
    screen <- getVideoSurface
    let step           = getStep g
        image          = getImage g
        coords         = getCoords g
        blockDimension = getBlockDimension g
        offset         = getBaseOffset g
    _ <- showImage image screen
    mapM_ (drawSegment screen blockDimension offset) $ take step coords
    SDL.flip screen

drawSegment :: Surface -> Dimension -> Dimension -> Coord -> IO ()
drawSegment screen blockDimension (xo, yo) (r, c) = do
    let (width, height) = blockDimension
        x = (c * width) + xo
        y = (r * height) + yo
    _ <- fillRect screen (Just (Rect x y width height)) (Pixel 0x000000)
    return ()

rangeCheck :: Ord a => a -> a -> a -> a
rangeCheck lowerBound upperBound n
    | n < lowerBound = lowerBound
    | n > upperBound = upperBound
    | otherwise      = n

doNextStep :: IORef Game -> (Int -> Int) -> IO ()
doNextStep gameRef f = do
    g <- readIORef gameRef
    let lastStep   = getStep g
        upperBound = maxSteps $ getConf g
        nextStep   = rangeCheck 0 upperBound $ f lastStep
    unless (nextStep == lastStep) $ do
        writeIORef gameRef g{ getStep = nextStep }
        display gameRef


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

handleKey :: IORef Game -> Event -> IO ()
handleKey game (SDL.KeyUp (Keysym SDLK_SPACE _ _))     = doNextStep game (subtract 1) >> eventLoop game
handleKey game (SDL.KeyUp (Keysym SDLK_BACKSPACE _ _)) = doNextStep game (+1) >> eventLoop game
handleKey game (SDL.KeyUp (Keysym SDLK_RETURN _ _))    = doNextStep game (*0) >> eventLoop game
handleKey game (SDL.KeyUp (Keysym SDLK_n _ _))         = nextRound game >> eventLoop game
handleKey game (SDL.KeyUp (Keysym SDLK_m _ _))         = reshuffle game >> eventLoop game
handleKey _    (SDL.KeyUp (Keysym SDLK_ESCAPE _ _))    = exit
handleKey game _                                       = return () >> eventLoop game

eventLoop :: IORef Game -> IO ()
eventLoop game = SDL.waitEventBlocking >>= handleKey game

exit :: IO ()
exit = do
    SDL.quit
    print "done..."

main :: IO ()
main = do
    SDL.init [InitEverything]
    conf <- getConfig
    config <- createAdjustedWindow "Recogs" conf
    coords <- shuffle $ coordinates (configRows config) (configCols config)
    game <- initGame config coords
    gameRef <- newIORef game
    display gameRef
    eventLoop gameRef

