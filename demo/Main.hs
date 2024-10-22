module Main where

import qualified Core.Algebra as CA
import qualified Core.Color as CC
import qualified Core.Image as CI

import qualified Graphics.Primitives as GP

import qualified Terminal.Buffer as TB
import qualified Terminal.Render as TR

exampleTransform :: TB.TerminalTransform
exampleTransform = TB.sequenceT [
        TB.putStringT TB.clearCommand,
        TB.putStringT TB.cursorToHomeCommand,
        TB.beginGraphicRendition [31],
        TB.putStringT "Hello, world!",
        TB.endGraphicRendition,
        TB.putStringT " ~~~~~"
    ]

exampleStaticImage :: TB.TerminalTransform
exampleStaticImage = TR.render 20 80 (CI.staticColor CC.magenta)

exampleTriangle :: TB.TerminalTransform
exampleTriangle = TR.render 20 80 img
    where img :: CI.Image
          img = GP.paint triangle CC.red (CI.staticColor CC.magenta)
          triangle :: GP.AbstractShape2D
          triangle = GP.polygon [CA.makePoint2D 0.5 1.0, CA.makePoint2D 0.0 0.0, CA.makePoint2D 1.0 0.0]

main :: IO ()
main = TB.run exampleTriangle
