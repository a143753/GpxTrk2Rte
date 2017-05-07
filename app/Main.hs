module Main where

import Text.XML.HXT.Core
import Data.String.Utils
import Data.List
import Data.List.Split
import System.Directory
import System.Environment

import Data.Monoid
import Data.Char
import Text.Read
import Options.Applicative

import Lib


sub _ _ [] = []
sub x y str@(s:ss)
    | isPrefixOf x str = y ++ drop (length x) str
    | otherwise = s:sub x y ss

convert split indir outdir trkfile = do
  let trkpath = indir ++ "/" ++ trkfile
      rtepath = outdir ++ "/" ++ (sub ".gpx" "_rt.gpx" trkfile)

  let doc = readDocument [withValidate no] trkpath

  nm <- runX (doc >>> getTrkName) -- get name tree
  ts <- runX (doc >>> getTrkSeg)  -- get trk segment

  let name = take split $ map (\ n -> (strip $ nm!!0) ++ "_" ++ (show n) ) [1..]     -- trk name                          -- split
      trks = map tplToTrkPt ts  -- trk points
      rtes = trkPtToRtePt trks  -- convert trk to rte
      decs = deci (250*split) rtes  -- decimate
      spts = map renum $ chunksOf 250 decs

  runX (
        root [] [writeRte name spts]
        >>>
        writeDocument [withIndent yes] rtepath                       -- 分割して書き出し
       )
  return ()

data Options = Options
  { indir  :: String
  , outdir :: String
  , split  :: Int
  } deriving Show

splitP :: Parser Int
splitP = option auto $ mconcat
  [ short 's'
  , long "split"
  , help "number of rte section"
  , metavar "INT"
  , value 1
  , showDefault
  ]

indirP :: Parser String
indirP = strOption $ mconcat
  [ short 'i'
  , long "indir"
  , help "input directory"
  , metavar "DIR"
  ]

outdirP :: Parser String
outdirP = strOption $ mconcat
  [ short 'o'
  , long "outdir"
  , help "output directory"
  , metavar "DIR"
  ]

optionsP :: Parser Options
optionsP = (<*>) helper $
  Options <$> indirP <*> outdirP <*> splitP

myParserInfo :: ParserInfo Options
myParserInfo = info optionsP $ mconcat 
  [ fullDesc
  , progDesc "test program."
  , header "header"
  , footer "footer"
  , progDesc "convert Gpx to Rte and decimate to 250 points"
  ]

main :: IO ()
main = do

  opt <- execParser myParserInfo

  let id = indir  opt
      od = outdir opt
      sp = Main.split  opt

  f <- getDirectoryContents id
  let trkfiles = filter (isSuffixOf ".gpx") f -- grab '*.gpx' files
  mapM_ (convert sp id od) trkfiles
  


