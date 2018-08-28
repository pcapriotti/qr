{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Array as A
import Data.Foldable (Foldable, asum)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Traversable (Traversable, traverse)
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

data Opts s = Opts
  { optVersion :: Maybe Version
  , optLevel :: Level
  , optMode :: Mode
  , optMDisplay :: Maybe Display
  , optOutput :: Maybe FilePath
  , optSize :: Maybe (Int, Int)
  , optSource :: s }
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

optDisplay :: Opts s -> Display
optDisplay args = case optMDisplay args of
  Just d -> d
  Nothing -> case optOutput args *> optSize args of
    Just _ -> Image
    Nothing -> Cairo

data Display = Cairo | Console | Image
  deriving (Eq, Ord, Read, Show)

data Source = Text String
            | File FilePath
            | StdIn
  deriving (Eq, Ord, Read, Show)

opts :: Parser (Opts Source)
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
  <*> (optional . option displayReader)
             ( long "display"
            <> short 'd'
            <> metavar "[cairo|console|image]"
            <> help "Display mode" )
  <*> (optional . option str)
             ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output file (implies image display)" )
  <*> (optional . option sizeReader)
             ( long "size"
            <> short 's'
            <> metavar "WIDTH,HEIGHT"
            <> help (concat [ "Image width and height in pixels, "
                            , "comma separated "
                            , "(implies image display)" ]) )
  <*> src

displayReader :: ReadM Display
displayReader = eitherReader $ \s -> do
  case s of
    "cairo" -> return Cairo
    "console" -> return Console
    "image" -> return Image
    _ -> Left "Invalid display mode. Possible choices: cairo, console, image."

sizeReader :: ReadM (Int, Int)
sizeReader = eitherReader $ \s -> do
  case break (== ',') s of
    (x, ',':y) -> (,) <$> sread "width" x <*> sread "height" y
    _ -> Left "Invalid size."
  where
    sread w x = case reads x of
      [(v, "")] -> Right v
      _ -> Left ("Invalid " ++ w ++ ".")

fileSource :: FilePath -> Source
fileSource "-" = StdIn
fileSource s = File s

src :: Parser Source
src = asum
  [ option r ( long "file"
          <> short 'f'
          <> metavar "FILENAME"
          <> help ( "Filename containing the data to encode "
                 ++ "(use '-' for standard input)" ) )
  , Text <$> argument str ( metavar "TEXT" )
  , pure StdIn ]
  where
    r = fmap fileSource str

extractText :: Source -> IO String
extractText (Text t) = pure t
extractText StdIn = getContents
extractText (File f) = readFile f

matrix :: Opts String -> Maybe Matrix
matrix (Opts mv l m _ _ _ txt) = do
  v <- mv <|> minimumVersion l m (length txt)
  return $ layout v l (message v l m txt)

main :: IO ()
main = do
  args <- execParser $ info (opts <**> helper)
    ( progDesc "Show a QR code" )
  targs <- traverse extractText args
  case matrix targs of
    Nothing -> hPutStrLn stderr "Message too large for a QR code"
            >> exitWith (ExitFailure 1)
    Just m -> runGUI args (optDisplay args) m

runGUI :: Opts s -> Display -> Matrix -> IO ()
runGUI _ Console m = do
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
runGUI _ Cairo m = do
  _ <- initGUI
  window <- windowNew

  _ <- window `on` exposeEvent $
    drawWindow window m
  _ <- handleResize window

  _ <- onDestroy window mainQuit
  widgetShowAll window
  mainGUI
runGUI args Image m = do
  let (w, h) = fromMaybe (300, 300) (optSize args)
      fmt = Cairo.FormatARGB32
  Cairo.withImageSurface fmt w h $ \surface -> do
    Cairo.renderWith surface (drawMatrix w h m)
    Cairo.surfaceWriteToPNG surface (fromMaybe "out.png" (optOutput args))

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

drawMatrix :: Int -> Int -> Matrix -> Cairo.Render ()
drawMatrix wxi wyi m = do
  let (_, (xsize, ysize)) = A.bounds m
      multx = wxi `div` (xsize + 9)
      multy = wyi `div` (ysize + 9)
      mult = min multx multy
      offsetx = (wxi - mult * (xsize + 1)) `div` 2
      offsety = (wyi - mult * (ysize + 1)) `div` 2
      setColor Light = Cairo.setSourceRGB 1 1 1
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

drawWindow :: (MonadIO m, WidgetClass w)
           => w -> Matrix -> m Bool
drawWindow window m = liftIO $ do
  (wxi, wyi) <- liftIO $ widgetGetSize window
  cr <- widgetGetDrawWindow window
  renderWithDrawable cr (drawMatrix wxi wyi m)

  return True
