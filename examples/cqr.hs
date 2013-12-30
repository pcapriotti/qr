module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Array as A
import Data.IORef
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.UI.Gtk hiding (Target)
import Options.Applicative

import Data.QR.Layout
import Data.QR.Grouping
import Data.QR.Types

data Opts = Opts
  { optVersion :: Version
  , optLevel :: Level
  , optMode :: Mode
  , optText :: String }

opts :: Parser Opts
opts = Opts
  <$> option ( long "symversion"
            <> short 'V'
            <> metavar "NUMBER"
            <> help "Version of the QR code: 1 to 40" )
  <*> option ( long "level"
            <> short 'l'
            <> metavar "LEVEL"
            <> help "Error correction: L, M, Q (default) or H"
            <> value Q )
  <*> option ( long "mode"
            <> short 'm'
            <> metavar "MODE"
            <> help "Encoding mode: Numeric, Alpha or Byte (default)"
            <> value Byte )
  <*> argument str ( metavar "TEXT" )

matrix :: Opts -> Matrix
matrix (Opts v l m txt) = layout v l (message v l m txt)

main :: IO ()
main = do
  args <- execParser $ info (opts <**> helper)
    ( progDesc "Show a QR code" )
  runGUI $ matrix args

runGUI :: Matrix -> IO ()
runGUI m = do
  initGUI
  window <- windowNew

  window `on` exposeEvent $ do
    drawWindow window m
  handleResize window

  onDestroy window mainQuit
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
  let (_, (ysize, xsize)) = A.bounds m
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

--          Cairo.rectangle
--            (fromIntegral (offsetx + x * mult))
--            (fromIntegral (offsety + y * mult))
--            (fromIntegral mult)
--            (fromIntegral mult)
--          Cairo.setSourceRGB 0.3 0.3 0.3
--          Cairo.stroke

    -- background
    setColor Light
    Cairo.rectangle 0 0 (fromIntegral wxi) (fromIntegral wyi)
    Cairo.fill

    Cairo.setLineWidth 0.1
    forM_ (A.assocs m) $ \(p, t) -> do
      drawTile t p

--    Cairo.setSourceRGB 0.0 0.7 0.0
--    forM_ (zip [0 :: Int ..] (placement m)) $ \(i, (x, y)) -> do
--      Cairo.moveTo (fromIntegral (offsetx + x * mult))
--                   (fromIntegral (offsety + (y + 1) * mult))
--      Cairo.showText (show i)
  return True
