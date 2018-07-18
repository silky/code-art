module Main where
-- References:
-- 
-- https://hackage.haskell.org/package/implicit-0.0.5/docs/Graphics-Implicit.html
-- https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language

import           Graphics.Implicit
import           Graphics.Implicit.Definitions
import           Control.Monad                 (join)
import           Data.List                     (isPrefixOf)
import           Options.Applicative
import           Data.String.Conv
import qualified Data.ByteString               as B
import qualified Options.Applicative           as A
import           System.FilePath.Find          hiding (directory)


dummyLineData = 
    [ [5, 6, 7, 0, 0, 10, 12, 43, 32, 26 ]
    , [2, 3, 0, 10, 11, 15, 30, 40, 20, 10, 25 ]
    ]


main :: IO ()
main = join $ execParser optsInfo


optsInfo :: ParserInfo (IO ())
optsInfo = info (helper <*> opts)
  ( fullDesc
    <> header "Generate some code-art for a folder." )


opts :: Parser (IO ())
opts = go <$> dirArg


dirArg :: Parser String
dirArg = A.strOption
  ( long "dir"
  <> short 'd'
  <> value "."
  <> metavar "DIRECTORY"
  <> help "The directory to process. Default: \".\"." )


go :: String -> IO ()
go dir = do
  lss <- lineLengthsInDirectory dir [".hs"]
  writeSCAD3 1 "out.scad" (art lss)


art :: [[ℝ]] -> SymbolicObj3
art lineData = union $ zipWith (\y -> union . (zipWith (barAt y) [0..])) [0..] lineData
  where
    width       = 5
    bar h       = rect3R 0 (0, 0, 0) (width, width, h)
    barAt x y h = translate (x * width, y * width, 0) (bar h)


lineLengths :: FilePath -> IO [ℝ]
lineLengths file = do
  content <- B.readFile file
  return $ map (fromIntegral . length) (lines (toS content))


lineLengthsInDirectory :: String -> [String] -> IO [[ℝ]]
lineLengthsInDirectory dir exts = do
  files  <- find always extsP dir
  mapM lineLengths files
    where
      notPrefix = liftOp (\a b -> not (isPrefixOf a b))
      extsP' = foldl (\p ext -> p ||? (extension ==? ext)) (return False) exts
      extsP = extsP' &&? (fileName `notPrefix` ".")

