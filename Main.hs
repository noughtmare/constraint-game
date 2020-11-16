{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative            as GtkDecl
import           GI.Gtk.Declarative.EventSource ( fromCancellation
                                                , Subscription
                                                )
import           GI.Gtk                         ( Label(..)
                                                , Window(..)
                                                , DrawingArea(..)
                                                )
import           Pipes
import qualified Pipes.Extras                  as Pipes

import qualified GI.Cairo
import qualified Graphics.Rendering.Cairo      as Cairo
import qualified Graphics.Rendering.Cairo.Internal
                                               as CairoInternal
import           Control.Monad.Trans.Reader
import           Foreign.Ptr
import           Data.GI.Base.ManagedPtr
import qualified Data.Vector                   as Vector
import qualified Diagrams.Backend.Cairo        as DiagramsCairo
import           Diagrams.Backend.Cairo         ( B )
import qualified Diagrams.Backend.Cairo.Internal
                                               as DiagramsCairoInternal

import           Diagrams.Prelude              as Diagrams
import           Diagrams.Core.Compile          ( renderDia )
import           Control.Lens            hiding ( (#) )
import           Data.Foldable                  ( fold )

import qualified GI.Gdk                        as Gdk
import           Data.Maybe                     ( fromMaybe )
import qualified Data.GI.Base                  as GI
import qualified Data.GI.Base.Signals          as GI

import qualified Data.Massiv.Array             as Massiv
import           Data.Foldable

import           Graphics.SVGFonts

-- import qualified Debug.Trace                   as Debug

data State = State (Double, Double) Int (Massiv.Matrix Massiv.U Int)
data Event = Nop | SetDigit !Int | Click !Double !Double | Resize !Double !Double | Closed

data RowCol = RowCol Int Int deriving (Eq, Ord, Show)
instance IsName RowCol

-- boards:
--   - filled       Int
--   - fixed        Bool
--   - conflicts    Bool
--   - pencilmarks  IntSet
--   - diagrams     Diagram B

renderedDigits :: Massiv.Vector Massiv.B (Diagram B)
renderedDigits = Massiv.makeArray
  Massiv.Seq
  (Massiv.Sz1 10)
  (\d -> stroke (textSVG (show d) 1) & fc black & lcA transparent)

numbers :: Massiv.Matrix Massiv.U Int -> Diagram B
numbers arr = (uncurry atPoints)
  (unzip
    [ ( p2 (fromIntegral r - 5, fromIntegral c - 5)
      , if d > 0 then renderedDigits Massiv.! d else mempty
      )
    | r <- [1 .. 9]
    , c <- [1 .. 9]
    , let d = arr Massiv.! Massiv.Ix2 (r - 1) (c - 1)
    ]
  )

board :: Diagram B
board =
  pad 1.1
    $  (uncurry atPoints)
         (unzip
           [ ( p2 (fromIntegral r - 5, fromIntegral c - 5)
             , square 1 & named (RowCol r c)
             )
           | r <- [1 .. 9]
           , c <- [1 .. 9]
           ]
         )
    <> (uncurry atPoints)
         (unzip
           [ ( p2 (fromIntegral (r - 2) * 3, fromIntegral (c - 2) * 3)
             , square 3 & lw thick
             )
           | r <- [1 .. 3]
           , c <- [1 .. 3]
           ]
         )

type DrawHandler = Cairo.Render ()
data DrawingAreaEvent
  = DrawingAreaClick  !Double !Double
  | DrawingAreaResize !Double !Double

drawingArea
  :: Vector.Vector (GtkDecl.Attribute DrawingArea DrawingAreaEvent)
  -> DrawHandler
  -> Widget DrawingAreaEvent
drawingArea customAttributes customParams = Widget CustomWidget { .. } where
  -- This function bridges gi-cairo with the hand-written cairo
  -- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
  -- and a `Render` action (as in the cairo lib), and renders the
  -- `Render` action into the given context.
  renderWithContext :: GI.Cairo.Context -> Cairo.Render () -> IO ()
  renderWithContext ct r = withManagedPtr ct $ \p ->
    runReaderT (CairoInternal.runRender r) (CairoInternal.Cairo (castPtr p))

  customWidget = DrawingArea

  customCreate :: DrawHandler -> IO (DrawingArea, GI.SignalHandlerId)
  customCreate draw = do
    drawingArea <- GI.new DrawingArea
                          [#events GI.:= [Gdk.EventMaskButtonPressMask]]
    h <- GI.on drawingArea #draw \ctx -> True <$ renderWithContext ctx draw
    return (drawingArea, h)

  customPatch
    :: DrawHandler
    -> DrawHandler
    -> GI.SignalHandlerId
    -> CustomPatch DrawingArea GI.SignalHandlerId
  customPatch _ draw h = CustomModify \drawingArea -> do
    GI.disconnectSignalHandler drawingArea h
    h <- GI.on drawingArea #draw \ctx -> True <$ renderWithContext ctx draw
    #queueDraw drawingArea
    pure h

  customSubscribe
    :: DrawHandler
    -> GI.SignalHandlerId
    -> DrawingArea
    -> (DrawingAreaEvent -> IO ())
    -> IO Subscription
  customSubscribe draw _h drawingArea cb = do
    h1 <- GI.on
      drawingArea
      #buttonPressEvent
      \eventButton -> do
        w <- fromIntegral <$> #getAllocatedWidth drawingArea
        h <- fromIntegral <$> #getAllocatedHeight drawingArea
        x <- GI.get eventButton #x
        y <- GI.get eventButton #y
        cb (DrawingAreaClick (x - 0.5 * w) (0.5 * h - y))
        return True
    h2 <- GI.on
      drawingArea
      #sizeAllocate
      \rectangle -> do
        w <- fromIntegral <$> GI.get rectangle #width
        h <- fromIntegral <$> GI.get rectangle #height
        cb (DrawingAreaResize w h)
    pure
      (fromCancellation
        (traverse_ (GI.disconnectSignalHandler drawingArea) (h1 : [h2]))
      )

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

app :: App Window State Event
app = App { .. } where
  update :: State -> Event -> Transition State Event
  update s Closed = Exit
  update s@(State (w, h) d b) (Click x y) =
    let board' | w < h     = scaleUToX w board
               | otherwise = scaleUToY h board
        pos = safeHead
          [ RowCol r c
          | r <- [1 .. 9]
          , c <- [1 .. 9]
          , fromMaybe False
            $   (\sub -> inquire (getSub sub) (mkP2 x y))
            <$> lookupName (RowCol r c) board'
          ]
        b' = case pos of
          Just (RowCol r c) -> Massiv.withMArrayST_
            b
            (\m -> Massiv.write m (Massiv.Ix2 (r - 1) (c - 1)) d)
          Nothing -> b
    in  Transition (State (w, h) d b') (pure Nothing)
  update (State _ d b) (Resize w h) =
    Transition (State (w, h) d b) (pure Nothing)
  update (State sz _ b) (SetDigit d) = Transition (State sz d b) (pure Nothing)
  update s              Nop          = Transition s (pure Nothing)

  view :: State -> AppView Window Event
  view s@(State (w, h) _ b) =
    bin
        Window
        [ #title := "Constraint Game"
        , on #hide Closed
        , #events := [Gdk.EventMaskKeyPressMask]
        , onM
          #keyPressEvent
          \eventKey drawingArea -> do
            keyVal <- fromIntegral <$> GI.get eventKey #keyval
            let v = keyVal - 48
            if 0 <= v && v < 10
              then pure (True, SetDigit v)
              else pure (True, Nop)
        ]
      $ fmap fromDrawingAreaEvent
      $ drawingArea
          []
          (snd $ renderDia
            DiagramsCairo.Cairo
            (DiagramsCairoInternal.CairoOptions "frame"
                                                (dims2D w h)
                                                DiagramsCairo.RenderOnly
                                                False
            )
            (numbers b <> board)
          )

  fromDrawingAreaEvent :: DrawingAreaEvent -> Event
  fromDrawingAreaEvent e = case e of
    DrawingAreaClick  x y -> Click x y
    DrawingAreaResize w h -> Resize w h

  inputs :: [Producer Event IO ()]
  inputs = []

  initialState :: State
  initialState = State
    (500, 500)
    0
    (Massiv.computeAs Massiv.U (Massiv.replicate Massiv.Seq (Massiv.Sz2 9 9) 0))

main :: IO ()
main = renderedDigits `seq` () <$ run app
