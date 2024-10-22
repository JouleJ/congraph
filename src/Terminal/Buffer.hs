module Terminal.Buffer where

import qualified Core.Algebra as CA
import qualified Core.Color as CC
import qualified Core.Metric as CM
import qualified Data.Char as DC
import qualified Data.List as DL

escapeChar :: Char
escapeChar = DC.chr 27

clearCommand :: String
clearCommand = escapeChar : "[2J"

cursorToHomeCommand :: String
cursorToHomeCommand = escapeChar : "[H"

type TerminalBuffer = String
type TerminalTransform = TerminalBuffer -> TerminalBuffer

run :: TerminalTransform -> IO ()
run t = putStr . reverse . t $ ""

putCharT :: Char -> TerminalTransform
putCharT = (:)

sequenceT :: [TerminalTransform] -> TerminalTransform
sequenceT [] = \buf-> buf
sequenceT (t:ts) = sequenceT ts . t

putStringT :: String -> TerminalTransform
putStringT s = sequenceT $ map putCharT s

putLineT :: String -> TerminalTransform
putLineT l = putCharT '\n' . putStringT l

beginGraphicRendition :: [Int] -> TerminalTransform
beginGraphicRendition xs = sequenceT [putCharT escapeChar, putCharT '[', putStringT formattedValues, putCharT 'm']
    where formattedValues :: String
          formattedValues = DL.intercalate ";" (map show xs)

endGraphicRendition :: TerminalTransform
endGraphicRendition = beginGraphicRendition [0]

colorCodeTable :: [(CC.Color, Int, Int)] -- (color, foregroundCode, backgroundCode)
colorCodeTable = [
        (CC.black, 30, 40),
        (CC.red, 31, 41),
        (CC.green, 32, 42),
        (CC.yellow, 33, 43),
        (CC.blue, 34, 44),
        (CC.magenta, 35, 45),
        (CC.cyan, 36, 46),
        (CC.white, 37, 47),
        (CC.brightYellow, 93, 103)
    ]

findNearestColorCode :: CC.Color -> (Int, Int) -- (foregroundCode, backgroundCode)
findNearestColorCode c = CA.findMinFoldable $ map (\(c', f, b) -> (CM.distance c c', (f, b))) colorCodeTable
