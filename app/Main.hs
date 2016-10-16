module Main where

import Text.XML.HXT.Core
import Lib

main :: IO ()
main = do
--  res1 <- runX (plotTree "/home/funamoto/Brevet/BRM922_PC1_dcm.gpx" "BRM922_PC1_dcm.tree")
--  print res1
--  res2 <- runX (plotTree "/home/funamoto/Brevet/BRM922_PC1_rt.gpx"  "BRM922_PC1_tr.tree")
--  print res2
  res3 <- runX (readTrk "/home/funamoto/Brevet/BRM922_PC1_dcm.gpx")
  let res4 = map tplToTrkPt res3
      res5 = trkPtToRtePt res4
  print $ show $ res4!!0
  print $ show $ res5!!0
  print $ length res4
  runX (
        root [] [writeRte res5]
--        readDocument [] "/home/funamoto/Brevet/BRM922_PC1_rt.gpx"
        >>>
--        writeDocument [withIndent yes] "/home/funamoto/BRM922_PC1_dcm.gpx")
        writeDocument [withIndent yes] "/home/funamoto/BRM922_PC1_dcm.gpx")
  return ()
