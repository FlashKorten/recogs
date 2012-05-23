import Graphics.Rendering.OpenGL
import Data.IORef
import Graphics.UI.GLUT  as GLUT
import RandomList
import Control.Monad.Random
import System.Exit ( exitSuccess )
import Config (getConfig, Config, configRows, configCols)

type Coord = (Int, Int)
data Game = Game { getCoords :: [Coord]
                 , getStep   :: Int
                 , getConf   :: Config
                 }

_WIDTH :: GLfloat
_WIDTH = 1

_HEIGHT :: GLfloat
_HEIGHT = 1

_FOREGROUND_DEPTH = 0.2
_BACKGROUND_DEPTH = 0.8

_BLACK = Color4 0 0 0 1
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

initGame :: Config -> [Coord] -> Game
initGame c l = Game { getConf   = c
                    , getCoords = l
                    , getStep   = length l
                    }

shuffleCoords :: [Coord] -> IO [Coord]
shuffleCoords = evalRandIO . permute

reshuffle game = do
    g <- get game
    let conf = getConf g
        rows = configRows conf
        cols = configCols conf
    c <- shuffleCoords $ coordinates rows cols
    game $= g{getCoords=c}
    display game

main = do
    (progName,_) <- getArgsAndInitialize
    conf <- getConfig
    initialDisplayMode $= [WithDepthBuffer,DoubleBuffered]
    createWindow progName
    setupProjection
    depthFunc $= Just Less
    coords <- shuffleCoords $ coordinates (configRows conf) (configCols conf)
    game <- newIORef $ initGame conf coords
    displayCallback $= display game
    -- idleCallback $= Just (idle game)
    reshapeCallback $= Just (reshape game)
    keyboardMouseCallback $= Just (keyboard game)
    mainLoop

display :: IORef Game -> IO ()
display game = do
    clearColor $= _BLACK
    clear [DepthBuffer,ColorBuffer]
    displayBackground
    displayForeground game
    swapBuffers

displayBackground :: IO ()
displayBackground = do
    currentColor $= _RED
    singleSegment 0 0 _WIDTH _HEIGHT _BACKGROUND_DEPTH

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

-- idle game = do
--     g <- get game
--     postRedisplay Nothing

limitStepCount max n
    | n < 0     = 0
    | n > max   = max
    | otherwise = n

doNextStep game changeStep = do
    g <- get game
    let nextStep = limitStepCount (maxSteps $ getConf g) $ changeStep $ getStep g
    game $= g{getStep = nextStep}
    display game

keyboard game (Char 'n')   Down _ _ = doNextStep game (flip (-) 1)
keyboard game (Char 'b')   Down _ _ = doNextStep game (+1)
keyboard game (Char 'c')   Down _ _ = doNextStep game (*0)
keyboard game (Char 's')   Down _ _ = reshuffle game
keyboard game (Char '\27') Down _ _ = exitSuccess
keyboard _ _ _ _ _                  = return ()

positionForSize :: Size -> Position
positionForSize (Size w h) = Position (-fromIntegral w`div`2) (-fromIntegral h`div`2)

fixSize :: Size -> Size
fixSize (Size w h) = Size (2*w) (2*h)

reshape game s = do
    screen <- get screenSize
    -- let size = fixSize screen
    let size = fixSize (Size 800 600)
    viewport $= (positionForSize size, size)

singleSegment x1 y1 x2 y2 z =
    renderPrimitive Quads $ mapM_ makeVertices points
        where makeVertices (x, y) = vertex $ Vertex3 x y z
              points = [(x1,y1),(x2,y1),(x2,y2),(x1,y2)]

setupProjection = do
  matrixMode $= Projection
  loadIdentity
  ortho 0 (realToFrac _WIDTH) 0 (realToFrac _HEIGHT) 0 1
  matrixMode $= Modelview 1
  loadIdentity

