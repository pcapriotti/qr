module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Array as A
import Data.IORef
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.UI.Gtk hiding (Display, Target)
import Options.Applicative
import System.Exit
import System.IO

import Data.QR.Encode
import Data.QR.Layout
import Data.QR.Grouping
import Data.QR.Types

data Opts = Opts
  { optVersion :: Maybe Version
  , optLevel :: Level
  , optMode :: Mode
  , optDisplay :: Display
  , optText :: Maybe String }

data Display = Cairo | Console

opts :: Parser Opts
opts = Opts
  <$> (optional . option auto)
             ( long "symversion"
            <> short 'V'
            <> metavar "NUMBER"
            <> help "Version of the QR code: 1 to 40 (default: auto)" )
  <*> option auto
             ( long "level"
            <> short 'l'
            <> metavar "LEVEL"
            <> help "Error correction: L, M, Q (default) or H"
            <> value Q )
  <*> option auto
             ( long "mode"
            <> short 'm'
            <> metavar "MODE"
            <> help "Encoding mode: Numeric, Alpha or Byte (default)"
            <> value Byte )
  <*> option displayReader
             ( long "display"
            <> short 'd'
            <> metavar "[cairo|console]"
            <> help "Display mode"
            <> value Cairo )
  <*> (optional . argument str) ( metavar "TEXT" )

displayReader :: ReadM Display
displayReader = eitherReader $ \s -> do
  case s of
    "cairo" -> return Cairo
    "console" -> return Console
    _ -> Left "Invalid display mode. Possible choices: cairo, console."

matrix :: Opts -> IO (Maybe Matrix)
matrix (Opts mv l m _ mtxt) = do
  txt <- maybe getContents return mtxt
  return $ do
    v <- mv <|> minimumVersion l m (length txt)
    return $ layout v l (message v l m txt)

main :: IO ()
main = do
  args <- execParser $ info (opts <**> helper)
    ( progDesc "Show a QR code" )
  mm <- matrix args
  case mm of
    Nothing -> hPutStrLn stderr "Message too large for a QR code"
            >> exitWith (ExitFailure 1)
    Just m -> runGUI (optDisplay args) m

runGUI :: Display -> Matrix -> IO ()
runGUI Console m = do
  let (_, (xsize, ysize)) = A.bounds m
  let white = putStr "\ESC[47m  \ESC[0m"
  let black = putStr "\ESC[40m  \ESC[0m"
  replicateM_ (xsize + 3) white
  putChar '\n'
  forM_ [0 .. ysize] $ \y -> do
    white
    forM_ [0 .. xsize] $ \x -> do
      let c = m A.! (x, y)
      if c == Dark then black else white
    white
    putChar '\n'
  replicateM_ (xsize + 3) white
  putChar '\n'
runGUI Cairo m = do
  _ <- initGUI
  window <- windowNew

  _ <- window `on` exposeEvent $
    drawWindow window m
  _ <- handleResize window

  _ <- onDestroy window mainQuit
  widgetShowAll window
  mainGUI

handleResize :: WindowClass w
             => w -> IO (ConnectId w)
handleResize window = do
  current <- windowGetSize window >>= newIORef
  (window `on` configureEvent) . fmap (const False) . runMaybeT $ do
    old_sz <- liftIO $ readIORef current
    sz <- lift eventSize
    guard $ sz /= old_sz
    liftIO $ do
      widgetQueueDraw window
      writeIORef current sz

drawWindow :: (MonadIO m, WidgetClass w)
           => w -> Matrix -> m Bool
drawWindow window m = liftIO $ do
  let (_, (xsize, ysize)) = A.bounds m
  (wxi, wyi) <- liftIO $ widgetGetSize window
  let multx = wxi `div` (xsize + 9)
      multy = wyi `div` (ysize + 9)
      mult = min multx multy
      offsetx = (wxi - mult * (xsize + 1)) `div` 2
      offsety = (wyi - mult * (ysize + 1)) `div` 2
  cr <- widgetGetDrawWindow window
  renderWithDrawable cr $ do
    let setColor Light = Cairo.setSourceRGB 1 1 1
        setColor Dark = Cairo.setSourceRGB 0 0 0
        setColor Reserved = Cairo.setSourceRGB 0.0 0.0 0.8
        setColor Empty = Cairo.setSourceRGB 0.8 0.8 0.8
    let drawTile md (x, y) = do
          Cairo.rectangle
            (fromIntegral (offsetx + x * mult))
            (fromIntegral (offsety + y * mult))
            (fromIntegral mult)
            (fromIntegral mult)
          setColor md
          Cairo.fill

    -- background
    setColor Light
    Cairo.rectangle 0 0 (fromIntegral wxi) (fromIntegral wyi)
    Cairo.fill

    Cairo.setLineWidth 0.1
    forM_ (A.assocs m) $ \(p, t) -> drawTile t p

  return True
