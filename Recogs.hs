import Graphics.Rendering.OpenGL
import Data.IORef
import Graphics.UI.GLUT  as GLUT
import RandomList
import Control.Monad.Random
import System.Exit ( exitWith, ExitCode(ExitSuccess))

type Coord = (Int, Int)
data Game = Game { getCoords :: [Coord]
                 , getStep :: Int
                 }

_ROWS   = 4
_COLS   = 3
_MAX_STEPS = _ROWS * _COLS

fieldWidth :: GLfloat
fieldWidth  = _WIDTH  / fromIntegral _COLS
fieldHeight :: GLfloat
fieldHeight = _HEIGHT / fromIntegral _ROWS

_WIDTH :: GLfloat
_WIDTH = 1

_HEIGHT :: GLfloat
_HEIGHT = 1

_FOREGROUND_DEPTH = 0.2
_BACKGROUND_DEPTH = 0.8

_BLACK = Color4 0 0 0 1
_GREY  = Color4 0.3 0.3 0.3 1
_RED   = Color4 1 0 0 1

coordinates :: [Coord]
coordinates = [(r, c)| r <- [0..(_ROWS - 1)], c <- [0..(_COLS - 1)]]

initGame :: [Coord] -> Game
initGame l = Game { getCoords = l
                  , getStep = length l
                  }

shuffleCoords :: [Coord] -> IO [Coord]
shuffleCoords = evalRandIO . permute

reshuffle game = do
    g <- get game
    c <- shuffleCoords coordinates
    game $= g{getCoords=c}
    display game

main = do
    (progName,_) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer,DoubleBuffered]
    createWindow progName
    setupProjection
    depthFunc $= Just Less
    coords <- shuffleCoords coordinates
    game <- newIORef $ initGame coords
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
    let step     = getStep g
        coords   = getCoords g
    drawSegments $ take step coords

drawSegments :: [Coord] -> IO ()
drawSegments = foldr ((>>) . drawSegment) (return ())

drawSegment :: Coord -> IO ()
drawSegment (r, c) = do
    let x = fromIntegral c * fieldWidth
        y = fromIntegral r * fieldHeight
    currentColor $= _GREY
    translate $ Vector3 x y _FOREGROUND_DEPTH
    singleSegment 0 0 fieldWidth fieldHeight 0
    translate $ Vector3 (-x) (-y) (-_FOREGROUND_DEPTH)

-- idle game = do
--     g <- get game
--     postRedisplay Nothing

limitStepCount n
    | n < 0          = 0
    | n > _MAX_STEPS = _MAX_STEPS
    | otherwise      = n

doNextStep game changeStep = do
    g <- get game
    let nextStep = limitStepCount $ changeStep $ getStep g
    game $= g{getStep = nextStep}
    display game

keyboard game (Char 'n')   Down _ _ = doNextStep game (flip (-) 1)
keyboard game (Char 'b')   Down _ _ = doNextStep game (+1)
keyboard game (Char 'c')   Down _ _ = doNextStep game (*0)
keyboard game (Char 's')   Down _ _ = reshuffle game
keyboard game (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _ _ _ _ _                  = return ()

positionForSize :: Size -> Position
positionForSize (Size w h) = Position (-(fromIntegral w)`div`2) (-(fromIntegral h)`div`2)

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

