module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Array as A
import Data.IORef
import Data.Char
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.UI.Gtk hiding (Target)

import Layout

txt :: String
txt = "After placing the data and error correction bits into the matrix, the QR code specification requires that a mask pattern be applied to the data and error correction bits. The purpose of this step is to reduce the number of hard-to-read patterns in the matrix. Continue to the next section to learn about data masking."

main :: IO ()
main = do
  let v = 7
  let m = unmaskedMatrix v $ map (fromIntegral . ord) txt
  runGUI m

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
  let multx = wxi `div` (xsize + 1)
      multy = wyi `div` (ysize + 1)
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
          Cairo.rectangle
            (fromIntegral (offsetx + x * mult))
            (fromIntegral (offsety + y * mult))
            (fromIntegral mult)
            (fromIntegral mult)
          Cairo.setSourceRGB 0.3 0.3 0.3
          Cairo.stroke
    Cairo.setLineWidth 0.1
    forM_ (A.assocs m) $ \(p, t) -> do
      drawTile t p
--    Cairo.setSourceRGB 0.0 0.7 0.0
--    forM_ (zip [0 :: Int ..] (placement m)) $ \(i, (x, y)) -> do
--      Cairo.moveTo (fromIntegral (offsetx + x * mult))
--                   (fromIntegral (offsety + (y + 1) * mult))
--      Cairo.showText (show i)
  return True
