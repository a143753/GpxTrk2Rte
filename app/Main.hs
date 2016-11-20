module Main where

import Text.XML.HXT.Core
import Data.String.Utils
import Data.List
import System.Directory
import System.Environment
import Lib

sub _ _ [] = []
sub x y str@(s:ss)
    | isPrefixOf x str = y ++ drop (length x) str
    | otherwise = s:sub x y ss

convert indir outdir trkfile = do
  let trkpath = indir ++ "/" ++ trkfile
      rtepath = outdir ++ "/" ++ (sub ".gpx" "_rt.gpx" trkfile)

  let doc = readDocument [withValidate no] trkpath

  nm <- runX (doc >>> getTrkName) -- get name tree
  ts <- runX (doc >>> getTrkSeg)  -- get trk segment

  let name = strip $ nm!!0      -- trk name
      trks = map tplToTrkPt ts  -- trk points
      rtes = trkPtToRtePt trks  -- convert trk to rte
      decs = deci 250 rtes      -- decimate

  runX (
        root [] [writeRte name decs]
        >>>
        writeDocument [withIndent yes] rtepath
       )
  return ()

main :: IO ()
main = do

  a <- getArgs
  let indir  = a!!0 -- input directory
      outdir = a!!1 -- output directory

  f <- getDirectoryContents indir
  let trkfiles = filter (isSuffixOf ".gpx") f -- grab '*.gpx' files
--  mapM_ print trkfiles
  mapM_ (convert indir outdir) trkfiles
  


