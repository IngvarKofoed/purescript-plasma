module Main where

import Prelude
import Data.Array
import Color (Color, rgb, toHexString)
import Control.Monad.Eff (Eff, forE, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Number.Format (fixed, toString, toStringWith)
import Data.Tuple (Tuple(..))
import Graphics.Canvas (CANVAS, Context2D, beginPath, canvasElementToImageSource, fillRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, lineTo, moveTo, setFillStyle, stroke)
import Math (cos, floor, sin, sqrt)
import Partial.Unsafe (unsafePartial)
import Signal (Signal, constant, runSignal, (~>))
import Signal.DOM (keyPressed)
import Signal.Time (every)


-- | Represents a pixel with position and RGB value
data Pixel = Pixel { x :: Number, y :: Number, value :: Value }
instance showPixel :: Show Pixel where
  show (Pixel pixel) = "(" <> (toString pixel.x) <> ", " <> (toString pixel.y) <> ", " <> (show pixel.value) <> ")" 



-- | Represents rgb values in the 0..1 range
data Value = Value { r :: Number, g :: Number, b :: Number }
instance showValue :: Show Value where
  show (Value value) = "(" <> (toString value.r) <> ", " <> (toString value.g) <> ", " <> (toString value.b) <> ")"



-- | Represents a 2D position
data Coord = Coord { x :: Number, y :: Number }
instance showCoord :: Show Coord where
  show (Coord coord) = "(" <> (toString coord.x) <> ", " <> (toString coord.y) <> ")" 



-- | Normalizes the coord (>= 0) value into the range [-0.5; 0.5] given the max
coordNorm :: Number -> Number -> Number
coordNorm max coord = coord / max - 0.5



-- | FPS signal
fps :: Signal Number
fps = every 50.0



-- | Converts Number into a byte [0;255] Int
toByte :: Number -> Int
toByte v =
  unsafePartial $ fromJust $ fromNumber $ floor $ v * 255.0



-- | Converts an RGB value into a color
toColor :: Value -> Color 
toColor (Value v) =
  rgb (toByte v.r) (toByte v.g) (toByte v.b)



-- | Render a single pixel given a canvas context, width and height of pixel, position and color
renderPixel :: forall e. Context2D -> Number -> Number -> Number -> Number -> String -> Eff(canvas :: CANVAS | e) Unit
renderPixel ctx w h x y color = do
  setFillStyle color ctx # void
  fillRect ctx { x:w*x, y:w*y, w:w, h:h } # void



-- | Renders an arrays of pixels given a pixel render function
renderPixels :: forall e. (Number -> Number -> String -> Eff(canvas :: CANVAS | e) Unit) -> Array Pixel -> Eff(canvas :: CANVAS | e) Unit  
renderPixels rp pixels = 
  foreachE pixels \(Pixel p) -> rp p.x p.y (toHexString $ toColor p.value) # void



-- | Creates all descrete coordinates for a bounded 2D area given width and height.
createCoords :: Int -> Int -> Array Coord
createCoords width height = do 
  x <- 0 .. width
  y <- 0 .. height
  [Coord { x: toNumber x, y: toNumber y }]



-- | Calculates pixels given coordinates for the pixels and a plasma generator
calculatePixels :: Array Coord -> (Number -> Number -> Number -> Value) -> (Number -> Number) -> (Number -> Number) -> Number -> Array Pixel
calculatePixels coords plasma normX normY time =
  coords # map \(Coord c) -> Pixel { x: c.x, y: c.y, value: plasma (normX c.x) (normY c.y) time }
  


-- | Returns the intensity in the range [0..1] 
liniarSinusoid :: Number -> Number -> Number -> Number
liniarSinusoid x y time = (sin (x * 2.0 - (time * 0.001))) / 2.0 + 0.5



-- | Returns the intensity in the range [0..1] 
rotateAndZoomSinusoid :: Number -> Number -> Number -> Number
rotateAndZoomSinusoid x y time = sin(4.0 * (x * sin((time * 0.001) / 2.0) + y * cos((time * 0.001) / 3.0)) + (time * 0.001)) / 2.0 + 0.5



-- | Returns the intensity in the range [0..1] 
ringsSinusoid :: Number -> Number -> Number -> Number
ringsSinusoid x y time =
  let cx = x + 0.5 * sin((time * 0.001) / 5.0)
      cy = y + 0.5 * cos((time * 0.001) / 3.0)
  in sin(sqrt(100.0 * (cx*cx + cy*cy) + 1.0) + (time * 0.001)) / 2.0 + 0.5






main :: forall e. Eff (console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial $ fromJust $ mcanvas
  ctx <- getContext2D canvas

  let pixelWidth = 10.0
  let pixelHeight = 10.0
  let pixelsCountHorizontal = 50
  let pixelsCountVertical = 50

  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  let coordNormWidth =  coordNorm (width / pixelWidth)
  let coordNormHeight = coordNorm (height / pixelHeight)

  -- Configure a pixel render 
  let myRenderPixel = renderPixel ctx pixelWidth pixelHeight
  -- Configure a pixels render
  let myRenderPixels = renderPixels myRenderPixel

  -- Create our coordinate system for which to generate pixels for
  let coords = createCoords pixelsCountHorizontal pixelsCountVertical

  -- Configure plasma(s)
  let plasma1 = \x y time -> let v = liniarSinusoid x y time
                             in Value { r: v, g: v, b: v }

  let plasma2 = \x y time -> let v = rotateAndZoomSinusoid x y time
                             in Value { r: v, g: v, b: v }

  let plasma3 = \x y time -> let v = ringsSinusoid x y time
                             in Value { r: v, g: v, b: v }

  let plasma4 = \x y time -> let r = liniarSinusoid x y time
                                 g = rotateAndZoomSinusoid x y time
                                 b = ringsSinusoid x y time
                                 in Value { r: r, g: g, b: b }

  let plasma5 = \x y time -> let r = rotateAndZoomSinusoid x y time
                                 g = rotateAndZoomSinusoid x y (time * 1.2 + 1000.0)
                                 b = rotateAndZoomSinusoid x y (time * 0.8 + 5000.0)
                                 in Value { r: r, g: g, b: b }                                 

  -- Configure our pixel value calculator using plasma taking time
  let simplePixelCalculator = calculatePixels coords plasma5 coordNormWidth coordNormHeight

  
  let loop = fps ~> \t -> simplePixelCalculator t # myRenderPixels
  runSignal loop
