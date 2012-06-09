module Main where

import Graphics.Rendering.OpenGL
import Data.IORef ( IORef, newIORef )
import Graphics.UI.GLUT as GLUT
import Codec.Picture.Types ( PixelRGB8, Image(Image) )
import Data.Vector.Storable ( unsafeWith )
import System.Exit ( exitSuccess, exitFailure )
import Control.Monad ( unless )
import Recogs.Util.Config ( getConfig )
import Recogs.Util.ImageReader ( getImageData )
import Recogs.Util.RandomList ( shuffle )
import Recogs.Data
    ( Config(configCols, configFS, configHeight, configImages, configRows, configWidth)
    , Coord
    , Game(..)
    , TextureData(..)
    )

_WIDTH, _HEIGHT :: GLfloat
_WIDTH = 1
_HEIGHT = 1

_FOREGROUND_DEPTH, _BACKGROUND_DEPTH :: GLfloat
_FOREGROUND_DEPTH = 0.2
_BACKGROUND_DEPTH = 0.8

_BLACK :: Color4 GLclampf
_BLACK = Color4 0 0 0 1

_GREY, _WHITE :: Color4 GLfloat
_GREY  = Color4 0.3 0.3 0.3 1
_WHITE = Color4 1 1 1 1

maxSteps :: Config -> Int
maxSteps c = configRows c * configCols c

fieldWidth :: Config -> GLfloat
fieldWidth  c = _WIDTH  / fromIntegral (configCols c)
fieldHeight :: Config -> GLfloat
fieldHeight c = _HEIGHT / fromIntegral (configRows c)

coordinates :: Int -> Int -> [Coord]
coordinates rows cols = [(r, c)| r <- [0..(rows - 1)], c <- [0..(cols - 1)]]

initGame :: Config -> [Coord] -> TextureData -> Int -> Game
initGame c l t n = Game { getConf    = c
                        , getCoords  = l
                        , getStep    = 0
                        , getTexture = t
                        , getFileNr  = n
                        }

changeImage :: IORef Game -> (Int -> Int) -> IO ()
changeImage game f = do
    g <- get game
    let config    = getConf g
        imgFiles  = configImages config
        fileNr    = getFileNr g
        newFileNr = rangeCheck 0 (length imgFiles) $ f fileNr
    unless (fileNr == newFileNr) $ do
        textureData <- getTextureByIndex config newFileNr
        game $= g{ getTexture = textureData
                 , getFileNr  = newFileNr
                 }
        displayReshaped game textureData

nextRound :: IORef Game -> IO ()
nextRound game = do
    g <- get game
    let fileNr    = getFileNr g
        config    = getConf g
        imgFiles  = configImages config
    if fileNr == length imgFiles - 1
      then exitSuccess
      else do
        let rows      = configRows config
            cols      = configCols config
        c <- shuffle $ coordinates rows cols
        textureData <- getTextureByIndex config $ fileNr + 1
        game $= g{ getCoords  = c
                 , getFileNr  = fileNr + 1
                 , getStep    = length c
                 , getTexture = textureData
                 }
        displayReshaped game textureData

displayReshaped :: IORef Game -> TextureData -> IO ()
displayReshaped game textureData = do
    let w = fromIntegral $ getTextureWidth textureData
        h = fromIntegral $ getTextureHeight textureData
    reshape game (Size w h)
    display game

showBlackBuffer :: IO ()
showBlackBuffer = do
    clearColor $= _BLACK
    clear [DepthBuffer,ColorBuffer]
    swapBuffers

getTextureByIndex :: Config -> Int -> IO TextureData
getTextureByIndex c i = do
    showBlackBuffer
    let imageName = configImages c !! i
    image <- getImageData imageName
    case image of
      Left err  -> putStrLn err >> exitFailure
      Right img -> do
        let Image w h _ = img
        tex <- createTexture img
        return $ TextureData tex w h

reshuffle :: IORef Game -> IO ()
reshuffle game = do
    g <- get game
    let conf = getConf g
        rows = configRows conf
        cols = configCols conf
    c <- shuffle $ coordinates rows cols
    game $= g{getCoords=c}
    display game

display :: IORef Game -> IO ()
display game = do
    clearColor $= _BLACK
    clear [DepthBuffer,ColorBuffer]
    g <- get game
    let tex    = getTexture g
        step   = getStep g
        coords = getCoords g
        conf   = getConf g
        width  = fieldWidth conf
        height = fieldHeight conf
    -- Background
    currentColor $= _WHITE
    textureBinding Texture2D $= Just (getTextureObject tex)
    textureSegment 0 0 _WIDTH _HEIGHT _BACKGROUND_DEPTH
    -- Foreground
    textureBinding Texture2D $= Nothing
    mapM_ (drawSegment width height) $ take step coords
    swapBuffers

drawSegment ::  GLfloat -> GLfloat -> Coord -> IO ()
drawSegment width height (r, c) = do
    let x = fromIntegral c * width
        y = fromIntegral r * height
    currentColor $= _GREY
    translate $ Vector3 x y _FOREGROUND_DEPTH
    singleSegment 0 0 width height 0
    translate $ Vector3 (-x) (-y) (-_FOREGROUND_DEPTH)

textureSegment :: (Num a, TexCoordComponent a, VertexComponent a) => a -> a -> a -> a -> a -> IO ()
textureSegment x1 y1 x2 y2 z =
    renderPrimitive Quads $ mapM_ makeVertices points
        where makeVertices (x, y) = do texCoord $ TexCoord3 x (1-y) z
                                       vertex $ Vertex3 x y z
              points = [(x1,y1),(x2,y1),(x2,y2),(x1,y2)]

singleSegment :: VertexComponent a => a -> a -> a -> a -> a -> IO ()
singleSegment x1 y1 x2 y2 z =
    renderPrimitive Quads $ mapM_ makeVertices points
        where makeVertices (x, y) = vertex $ Vertex3 x y z
              points = [(x1,y1),(x2,y1),(x2,y2),(x1,y2)]

rangeCheck :: Ord a => a -> a -> a -> a
rangeCheck lowerBound upperBound n
    | n < lowerBound = lowerBound
    | n > upperBound = upperBound
    | otherwise      = n

doNextStep :: IORef Game -> (Int -> Int) -> IO ()
doNextStep game f = do
    g <- get game
    let lastStep   = getStep g
        upperBound = maxSteps $ getConf g
        nextStep   = rangeCheck 0 upperBound $ f lastStep
    unless (nextStep == lastStep) $
        game $= g{ getStep = nextStep }
    display game

keyboard :: IORef Game -> Key -> KeyState -> a -> b -> IO ()
keyboard game (Char 'n') Down _ _ = doNextStep game (subtract 1) -- show one more part of the image
keyboard game (Char 'b') Down _ _ = doNextStep game (+1)         -- show one less part of the image
keyboard game (Char 'c') Down _ _ = doNextStep game (*0)         -- show the whole image
keyboard game (Char 'd') Down _ _ = nextRound game               -- load the next image and conceal it
keyboard game (Char 's') Down _ _ = reshuffle game               -- show the same amount but different parts
keyboard game (Char 'N') Down _ _ = changeImage game (+1)
keyboard game (Char 'B') Down _ _ = changeImage game (subtract 1)
keyboard _  (Char '\27') Down _ _ = exitSuccess
keyboard _ _ _ _ _                = return ()

getOffset :: Int -> Int -> Int
getOffset outer inner
    | outer > inner = fromIntegral (outer - inner) `div` 2
    | otherwise     = 0

positionForSize :: Int -> Int -> Size -> Position
positionForSize screenW screenH (Size imageW imageH)
    = Position (wOffset + (-fromIntegral imageW `div`2))
               (hOffset + (-fromIntegral imageH `div`2))
         where wOffset  = fromIntegral $ getOffset screenW $ fromIntegral imageW `div` 2
               hOffset  = fromIntegral $ getOffset screenH $ fromIntegral imageH `div` 2

fixSize :: Size -> Size
fixSize (Size w h) = Size (2*w) (2*h)

calculateSize :: Int -> Int -> Int -> Int -> (Int, Int)
calculateSize texWidth texHeight screenWidth screenHeight
    = (floor $ factor * fromIntegral texWidth, floor $ factor * fromIntegral texHeight)
        where widthFactor  = f screenWidth texWidth
              heightFactor = f screenHeight texHeight
              factor       = min widthFactor heightFactor
              f :: Int -> Int -> Double
              f x y        = fromIntegral x / fromIntegral y

getDataFromSize :: Size -> (Int, Int)
getDataFromSize (Size w h) = (fromIntegral w, fromIntegral h)

reshape :: IORef Game -> a -> IO ()
reshape game _ = do
    g <- get game
    let config    = getConf g
        tex       = getTexture g
        textureW  = getTextureWidth tex
        textureH  = getTextureHeight tex
        screenW   = configWidth config
        screenH   = configHeight config
        (w, h)    = calculateSize textureW textureH screenW screenH
        imageSize = fixSize $ Size (fromIntegral w) (fromIntegral h)
    viewport $= (positionForSize screenW screenH imageSize, imageSize)

setupProjection :: IO ()
setupProjection = do
  matrixMode $= Projection
  loadIdentity
  ortho 0 (realToFrac _WIDTH) 0 (realToFrac _HEIGHT) 0 1
  matrixMode $= Modelview 1
  loadIdentity

createAdjustedWindow :: String -> Config -> IO Config
createAdjustedWindow name c
    | configFS c = do
                   _ <- createWindow name
                   fullScreen
                   screen <- get screenSize
                   let (w, h) = getDataFromSize screen
                   return c{ configWidth  = w
                           , configHeight = h
                           }
    | otherwise  = do
                   initialWindowSize $= Size (fromIntegral $ configWidth c) (fromIntegral $ configHeight c)
                   _ <- createWindow name
                   return c

main :: IO ()
main = do
    (progName,_) <- getArgsAndInitialize
    conf <- getConfig
    coords <- shuffle $ coordinates (configRows conf) (configCols conf)
    initialDisplayMode $= [RGBMode, WithDepthBuffer, DoubleBuffered]
    config <- createAdjustedWindow progName conf
    setupProjection
    depthFunc $= Just Less
    textureData <- getTextureByIndex conf 0
    game <- newIORef $ initGame config coords textureData 0
    displayCallback       $= display game
    reshapeCallback       $= Just (reshape game)
    keyboardMouseCallback $= Just (keyboard game)
    mainLoop

mapTexture :: Image PixelRGB8 -> IO ()
mapTexture (Image width height dat)
  = unsafeWith dat pointerFunc
    where pointerFunc ptr = texImage2D Nothing NoProxy 0 RGB8 texSize 0 (PixelData RGB UnsignedByte ptr)
          texSize = TextureSize2D (fromIntegral width) (fromIntegral height)

createTexture :: Image PixelRGB8 -> IO TextureObject
createTexture image = do
    clearColor $= Color4 0 0 0 0
    shadeModel $= Flat
    depthFunc  $= Just Less
    rowAlignment Unpack $= 1

    [imageTexture] <- genObjectNames 1
    textureBinding  Texture2D   $= Just imageTexture
    textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, Clamp)
    textureWrapMode Texture2D T $= (Repeated, Clamp)
    mapTexture image
    texture Texture2D $= Enabled
    return imageTexture
