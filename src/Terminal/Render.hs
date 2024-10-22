module Terminal.Render where

import Terminal.Buffer

import qualified Core.Algebra as CA
import qualified Core.Color as CC
import qualified Core.Image as CI

render :: Int -> Int -> CI.Image -> TerminalTransform
render rows cols img = sequenceT [putStringT clearCommand, putStringT cursorToHomeCommand, impl, endGraphicRendition]
    where pixels :: [Int] -- background codes
          pixels = map convertPixel . CA.sort . CI.imageToPixelGrid (rows - 1) (cols - 1) $ img
          convertPixel :: (a, CC.Color) -> Int
          convertPixel (_, c) = let (_, b) = findNearestColorCode c in b
          impl :: TerminalTransform
          impl = putByLines pixels
          putByLines :: [Int] -> TerminalTransform
          putByLines ps = sequenceT (map putLine (CA.chunksOf cols ps))
          putLine :: [Int] -> TerminalTransform
          putLine ps = putCharT '\n' . sequenceT (map (\b -> putCharT ' ' . beginGraphicRendition [b]) ps)
