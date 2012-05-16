import Graphics.Rendering.OpenGL
import Data.IORef
import Graphics.UI.GLUT  as GLUT
import RandomList
import Control.Monad.Random
import System.Exit ( exitWith, ExitCode(ExitSuccess))

type Koord = (Int, Int)
data Game = Game { getFields :: [Koord]
                 , getStep :: Int
                 }

_ROWS   = 4
_COLS   = 3
_MAX_STEPS = _ROWS * _COLS

fieldWidth :: GLfloat
fieldWidth  = (_WIDTH  / (fromIntegral _COLS))
fieldHeight :: GLfloat
fieldHeight = (_HEIGHT / (fromIntegral _ROWS))

_WIDTH :: GLfloat
_WIDTH = 1

_HEIGHT :: GLfloat
_HEIGHT = 1

_FOREGROUND_DEPTH = 0.2
_BACKGROUND_DEPTH = 0.8

fields :: [Koord]
fields = [(r, c)| r <- [0..(_ROWS - 1)], c <- [0..(_COLS - 1)]]

initGame :: [Koord] -> Game
initGame l = Game { getFields = l
                  , getStep = length l
                  }

shuffleKoords :: [Koord] -> IO [Koord]
shuffleKoords fields = do
    evalRandIO $ permute fields

main = do
    (progName,_) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer,DoubleBuffered]
    createWindow progName
    setupProjection
    depthFunc $= Just Less
    koords <- shuffleKoords fields
    game <- newIORef $ initGame koords
    displayCallback $= display game
    -- idleCallback $= Just (idle game)
    reshapeCallback $= Just (reshape game)
    keyboardMouseCallback $= Just (keyboard game)
    mainLoop

display :: IORef Game -> IO ()
display game = do
    clearColor $= Color4 0 0 0 1
    clear [DepthBuffer,ColorBuffer]
    displayBackground
    displayForeground game
    swapBuffers

displayBackground :: IO ()
displayBackground = do
    currentColor $= Color4 1 0 0 1
    schranktuer 0 0 _WIDTH _HEIGHT _BACKGROUND_DEPTH

displayForeground :: IORef Game -> IO ()
displayForeground game = do
    g <- get game
    let step   = getStep g
        fields = getFields g
    drawTueren $ take step fields

drawTueren :: [Koord] -> IO ()
drawTueren []     = return ()
drawTueren (x:xs) = drawTuer x >> drawTueren xs

drawTuer :: Koord -> IO ()
drawTuer (r, c) = do
    let x = (fromIntegral c) * fieldWidth
        y = (fromIntegral r) * fieldHeight
    currentColor $= Color4 0.3 0.3 0.3 1
    translate $ Vector3 x y _FOREGROUND_DEPTH
    schranktuer 0 0 fieldWidth fieldHeight _FOREGROUND_DEPTH
    translate $ Vector3 (-x) (-y) (-_FOREGROUND_DEPTH)

idle game = do
    g <- get game
    postRedisplay Nothing

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

schranktuer x1 y1 x2 y2 z = 
    renderPrimitive Quads $ mapM_ makeVertices points
        where makeVertices (x, y) = vertex $ Vertex3 x y z
              points = [(x1,y1),(x2,y1),(x2,y2),(x1,y2)]

setupProjection = do
  matrixMode $= Projection
  loadIdentity
  ortho 0 (realToFrac _WIDTH) 0 (realToFrac _HEIGHT) 0 1
  matrixMode $= Modelview 1 
  loadIdentity

