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
    conf    <- getConfig
    config  <- createAdjustedWindow "Recogs" conf
    coords  <- shuffle $ coordinates config
    game    <- initGame config coords
    gameRef <- newIORef game
    display gameRef
    eventLoop gameRef

eventLoop :: IORef Game -> IO ()
eventLoop game = SDL.waitEventBlocking >>= handleEvent game

handleEvent :: IORef Game -> Event -> IO ()
handleEvent _    (SDL.KeyUp (Keysym SDLK_ESCAPE _ _)) = exit
handleEvent game (SDL.KeyUp (Keysym k _ _))           = handleKey game k >> eventLoop game
handleEvent game _                                    = eventLoop game

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

coordinates :: Config -> [Coord]
coordinates config = [(r, c)| r <- [0..(rows - 1)], c <- [0..(cols - 1)]]
                        where rows = configRows config
                              cols = configCols config

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
    freeSurface image
    let offset    = getOffset scaledImageDimension screenDimension
        scaledImageDimension = getSurfaceDimension scaledImage
        blockDimension = calculateBlockDimension scaledImageDimension (configRows config) (configCols config)
    return (scaledImage, blockDimension, offset)

initImageData :: Config -> IO (Surface, Dimension, Dimension)
initImageData config = do
    image  <- load $ head $ configImages config
    calculateImageData image config

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

doNextStep :: IORef Game -> (Int -> Int) -> IO ()
doNextStep gameRef f = do
    game <- readIORef gameRef
    let lastStep   = getStep game
        upperBound = maxSteps $ getConf game
        nextStep   = rangeCheck 0 upperBound $ f lastStep
    unless (nextStep == lastStep) $ do
        writeIORef gameRef game {getStep = nextStep}
        display gameRef

nextRound :: IORef Game -> IO ()
nextRound gameRef = do
    game <- readIORef gameRef
    let fileNr    = getFileNr game
        config    = getConf game
        imgFiles  = configImages config
    if fileNr == length imgFiles - 1
      then exitSuccess
      else do
        let nextFileNr = fileNr + 1
        coords <- shuffle $ coordinates config
        image  <- load $ configImages config !! nextFileNr
        (scaledImage, blockDimension, offset) <- calculateImageData image config
        freeSurface image
        clearScreen
        writeIORef gameRef game { getCoords = coords
                                , getFileNr = nextFileNr
                                , getStep   = maxSteps config
                                , getImage  = scaledImage
                                , getBlockDimension = blockDimension
                                , getBaseOffset = offset
                                }

reshuffle :: IORef Game -> IO ()
reshuffle gameRef = do
    game <- readIORef gameRef
    coords <- shuffle $ coordinates $ getConf game
    writeIORef gameRef game {getCoords = coords}
    display gameRef

revealOrStartNext :: IORef Game -> IO ()
revealOrStartNext gameRef = do
    game <- readIORef gameRef
    if getStep game == 0
      then nextRound gameRef
      else doNextStep gameRef (*0)

clearScreen :: IO ()
clearScreen = do
    screen <- getVideoSurface
    _ <- fillRect screen (Just (Rect 0 0 (surfaceGetWidth screen) (surfaceGetHeight screen))) (Pixel 0x000000)
    SDL.flip screen

display :: IORef Game -> IO ()
display gameRef = do
    game <- readIORef gameRef
    screen <- getVideoSurface
    let step           = getStep game
        image          = getImage game
        coords         = getCoords game
        blockDimension = getBlockDimension game
        offset         = getBaseOffset game
    _ <- showImage image screen
    mapM_ (drawSegment screen blockDimension offset) $ take step coords
    SDL.flip screen

showImage :: Surface -> Surface -> IO Bool
showImage surface screen = blitSurface surface Nothing screen $ getOffsetRect (getSurfaceDimension surface) (getSurfaceDimension screen)

drawSegment :: Surface -> Dimension -> Dimension -> Coord -> IO ()
drawSegment screen blockDimension (xOffset, yOffset) (row, col) = do
    let (width, height) = blockDimension
        x               = (col * width)  + xOffset
        y               = (row * height) + yOffset
    _ <- fillRect screen (Just (Rect x y width height)) (Pixel 0x000000)
    return ()

createAdjustedWindow :: String -> Config -> IO Config
createAdjustedWindow title config
    | configFS config = do
                   info <- getVideoInfo
                   let w = videoInfoWidth info
                       h = videoInfoHeight info
                   _ <- setVideoMode w h 32 [Fullscreen]
                   return config { configWidth  = w
                                 , configHeight = h
                                 }
    | otherwise  = do
                   let w = fromIntegral $ configWidth config
                       h = fromIntegral $ configHeight config
                   _ <- setVideoMode w h 32 []
                   _ <- SDL.setCaption title title
                   return config

exit :: IO ()
exit = do
    SDL.quit
    print "done..."

