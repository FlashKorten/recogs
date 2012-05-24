import Data.Bits ( (.&.) )
import Foreign ( withArray )

import Graphics.Rendering.OpenGL
import Data.IORef
import Graphics.UI.GLUT  as GLUT
import RandomList
import Control.Monad.Random
import System.Exit ( exitSuccess )
import Config (getConfig, Config, configRows, configCols)

type Coord = (Int, Int)
data Game = Game { getCoords  :: [Coord]
                 , getStep    :: Int
                 , getConf    :: Config
                 , getTexture :: TextureObject
                 }

_WIDTH, _HEIGHT :: GLfloat
_WIDTH = 1
_HEIGHT = 1

_FOREGROUND_DEPTH, _BACKGROUND_DEPTH :: GLfloat
_FOREGROUND_DEPTH = 0.2
_BACKGROUND_DEPTH = 0.8

_BLACK :: Color4 GLclampf
_BLACK = Color4 0 0 0 1

_GREY, _RED :: Color4 GLfloat
_GREY  = Color4 0.3 0.3 0.3 1
_RED   = Color4 1 0 0 1

maxSteps :: Config -> Int
maxSteps c = configRows c * configCols c

fieldWidth :: Config -> GLfloat
fieldWidth  c = _WIDTH  / fromIntegral (configCols c)
fieldHeight :: Config -> GLfloat
fieldHeight c = _HEIGHT / fromIntegral (configRows c)

coordinates :: Int -> Int -> [Coord]
coordinates rows cols = [(r, c)| r <- [0..(rows - 1)], c <- [0..(cols - 1)]]

initGame :: Config -> [Coord] -> TextureObject -> Game
initGame c l t = Game { getConf    = c
                      , getCoords  = l
                      , getStep    = length l
                      , getTexture = t
                      }

shuffleCoords :: [Coord] -> IO [Coord]
shuffleCoords = evalRandIO . permute

reshuffle :: IORef Game -> IO ()
reshuffle game = do
    g <- get game
    let conf = getConf g
        rows = configRows conf
        cols = configCols conf
    c <- shuffleCoords $ coordinates rows cols
    game $= g{getCoords=c}
    display game

display :: IORef Game -> IO ()
display game = do
    clearColor $= _BLACK
    clear [DepthBuffer,ColorBuffer]
    displayBackground game
    displayForeground game
    swapBuffers

displayBackground :: IORef Game -> IO ()
displayBackground game = do
    g <- get game
    let tex = getTexture g
    textureBinding Texture2D $= Just tex
    currentColor $= _RED
    textureSegment 0 0 _WIDTH _HEIGHT _BACKGROUND_DEPTH

displayForeground :: IORef Game -> IO ()
displayForeground game = do
    g <- get game
    let step   = getStep g
        coords = getCoords g
        conf   = getConf g
        width  = fieldWidth conf
        height = fieldHeight conf
    drawSegments width height  $ take step coords

drawSegments :: GLfloat -> GLfloat -> [Coord] -> IO ()
drawSegments width height = foldr ((>>) . drawSegment width height) (return ())

drawSegment ::  GLfloat -> GLfloat -> Coord -> IO ()
drawSegment width height (r, c) = do
    let x = fromIntegral c * width
        y = fromIntegral r * height
    currentColor $= _GREY
    translate $ Vector3 x y _FOREGROUND_DEPTH
    singleSegment 0 0 width height 0
    translate $ Vector3 (-x) (-y) (-_FOREGROUND_DEPTH)

textureSegment :: (TexCoordComponent a, VertexComponent a) => a -> a -> a -> a -> a -> IO ()
textureSegment x1 y1 x2 y2 z =
    renderPrimitive Quads $ mapM_ makeVertices points
        where makeVertices (x, y) = do texCoord $ TexCoord3 x y z
                                       vertex $ Vertex3 x y z
              points = [(x1,y1),(x2,y1),(x2,y2),(x1,y2)]

singleSegment :: VertexComponent a => a -> a -> a -> a -> a -> IO ()
singleSegment x1 y1 x2 y2 z =
    renderPrimitive Quads $ mapM_ makeVertices points
        where makeVertices (x, y) = vertex $ Vertex3 x y z
              points = [(x1,y1),(x2,y1),(x2,y2),(x1,y2)]

-- idle game = do
--     g <- get game
--     postRedisplay Nothing

rangeCheck :: Ord a => a -> a -> a -> a
rangeCheck lowerBound upperBound n
    | n < lowerBound = lowerBound
    | n > upperBound = upperBound
    | otherwise      = n

doNextStep :: IORef Game -> (Int -> Int) -> IO ()
doNextStep game changeStep = do
    g <- get game
    let nextStep = rangeCheck 0 (maxSteps $ getConf g) $ changeStep $ getStep g
    game $= g{getStep = nextStep}
    display game

keyboard :: IORef Game -> Key -> KeyState -> a -> b -> IO ()
keyboard game (Char 'n') Down _ _ = doNextStep game (flip (-) 1)
keyboard game (Char 'b') Down _ _ = doNextStep game (+1)
keyboard game (Char 'c') Down _ _ = doNextStep game (*0)
keyboard game (Char 's') Down _ _ = reshuffle game
keyboard _  (Char '\27') Down _ _ = exitSuccess
keyboard _ _ _ _ _                = return ()

positionForSize :: Size -> Position
positionForSize (Size w h) = Position (-fromIntegral w`div`2) (-fromIntegral h`div`2)

fixSize :: Size -> Size
fixSize (Size w h) = Size (2*w) (2*h)

reshape :: t -> IO ()
reshape _ = do
    -- screen <- get screenSize
    -- let size = fixSize screen
    let size = fixSize (Size 800 600)
    viewport $= (positionForSize size, size)

setupProjection :: IO ()
setupProjection = do
  matrixMode $= Projection
  loadIdentity
  ortho 0 (realToFrac _WIDTH) 0 (realToFrac _HEIGHT) 0 1
  matrixMode $= Modelview 1
  loadIdentity

main :: IO ()
main = do
    (progName,_) <- getArgsAndInitialize
    conf <- getConfig
    initialDisplayMode $= [RGBMode, WithDepthBuffer, DoubleBuffered]
    _ <- createWindow progName
    setupProjection
    depthFunc $= Just Less
    coords <- shuffleCoords $ coordinates (configRows conf) (configCols conf)
    texNames <- createTexture
    game <- newIORef $ initGame conf coords texNames
    displayCallback $= display game
    -- idleCallback $= Just (idle game)
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboard game)
    mainLoop

-- Create checkerboard image
checkImageSize :: TextureSize2D
checkImageSize = TextureSize2D 64 64

withCheckImage :: TextureSize2D -> GLsizei -> (GLubyte -> Color4 GLubyte)
               -> (PixelData (Color4 GLubyte) -> IO ()) -> IO ()
withCheckImage (TextureSize2D w h) n f act =
   withArray [ f c |
               i <- [ 0 .. w - 1 ],
               j <- [ 0 .. h - 1 ],
               let c | (i .&. n) == (j .&. n) = 0
                     | otherwise              = 255 ] $
   act . PixelData RGBA UnsignedByte

createTexture :: IO TextureObject
createTexture = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat
   depthFunc $= Just Less
   rowAlignment Unpack $= 1

   [imageTexture] <- genObjectNames 1
   textureBinding Texture2D $= Just imageTexture
   textureWrapMode Texture2D S $= (Repeated, Clamp)
   textureWrapMode Texture2D T $= (Repeated, Clamp)
   textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
   withCheckImage checkImageSize 0x08 (\c -> Color4 c c c 255) $
      texImage2D Nothing NoProxy 0  RGBA' checkImageSize 0
   texture Texture2D $= Enabled
   return imageTexture

