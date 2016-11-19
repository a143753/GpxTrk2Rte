module Main where

import Text.XML.HXT.Core
import Data.String.Utils
import Lib

main :: IO ()
main = do
--  res1 <- runX (plotTree "/home/funamoto/Brevet/BRM922_PC1_dcm.gpx" "BRM922_PC1_dcm.tree")
--  print res1
--  res2 <- runX (plotTree "/home/funamoto/Brevet/BRM922_PC1_rt.gpx"  "BRM922_PC1_tr.tree")
--  print res2

  let doc = readDocument [withValidate no] "/home/funamoto/Brevet/BRM922_PC1.gpx"

  nm <- runX (doc >>> getTrkName)
  ts <- runX (doc >>> getTrkSeg)

  let name = strip $ nm!!0
      trks = map tplToTrkPt ts
      rtes = trkPtToRtePt trks
      decs = deci 250 rtes

--  let xys  = map (\a -> (rteLat a, rteLon a) ) rtes
  mapM_ print decs
      
  runX (
        root [] [writeRte name decs]
        >>>
        writeDocument [withIndent yes] "/home/funamoto/BRM922_PC1_rt.gpx")
  return ()
