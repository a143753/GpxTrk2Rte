module Lib
    ( plotTree
    , readTrk
    , tplToTrkPt
    , trkPtToRtePt
    , writeRte
    ) where

import Text.XML.HXT.Core
--import Text.XML.HXT.Curl

-- (lat,lon,ele,time)
data TrkPt = TrkPt Double Double Double String deriving(Show)
trkLat (TrkPt lat _ _ _) = lat
trkLon (TrkPt _ lon _ _) = lon
trkEle (TrkPt _ _ ele _) = ele
trkTim (TrkPt _ _ _ tim) = tim

-- (name, lat,lon,ele,time)
data RtePt = RtePt String Double Double Double String deriving(Show)
rteNam (RtePt nam _ _ _ _) = nam
rteLat (RtePt _ lat _ _ _) = lat
rteLon (RtePt _ _ lon _ _) = lon
rteEle (RtePt _ _ _ ele _) = ele
rteTim (RtePt _ _ _ _ tim) = tim

plotTree :: FilePath -> FilePath -> IOSArrow XmlTree XmlTree
plotTree infile outfile = 
    readDocument [withValidate no] infile >>>
    putXmlTree outfile

tplToTrkPt :: (String,(String,(String,String))) -> TrkPt
tplToTrkPt (a,(b,(c,d))) = TrkPt (read a) (read b) (read c) d

trkPtToRtePt :: [TrkPt] -> [RtePt]
trkPtToRtePt tp = map f $ zip [1..] tp
    where
      f (n,p) = RtePt ("No. "++show(n)) (trkLat p) (trkLon p) (trkEle p) (trkTim p)

-- -> (lat,(lon,(ele,(time))))
readTrk :: FilePath -> IOSArrow XmlTree ( String, ( String, (String, String)))
readTrk filename = 
    readDocument [withValidate no] filename >>>
    getChildren >>>
    isElem >>> hasName "gpx" >>>
    getChildren >>>
    isElem >>> hasName "trk" >>>
    getChildren >>>
    isElem >>> hasName "trkseg" >>>
    getChildren >>>
    isElem >>> hasName "trkpt" >>>
    ( getAttrValue "lat" ) &&&
    ( getAttrValue "lon" ) &&&
    ( getChildren >>> isElem >>> hasName "ele"  >>> getChildren >>> getText ) &&&
    ( getChildren >>> isElem >>> hasName "time" >>> getChildren >>> getText )
                  
writeRte :: ArrowXml a => [RtePt] -> a XmlTree XmlTree
writeRte rts = mkelem "gpx"
               [ sattr "version" "1.1"
               , sattr "creator" "GpxTrk2Rte"
               , sattr "xmlns"   "http://www.topografix.com/GPX/1/1"
               , sattr "xsi:schemaLocation" "http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd http://www.kashmir3d.com/namespace/kashmir3d http://www.kashmir3d.com/namespace/kashmir3d.xsd"
               ]
               [ selem "rte"
                 ([ selem "name"   [ txt "BRM922 PC1" ]
                  , selem "number" [ txt "1" ]
                  ] ++ pts)
               ]
    where
      pts = map (\x -> mkelem "rtept"
                       [ sattr "lat"   (show $ rteLat x)
                       , sattr "lon"   (show $ rteLon x)
                       ]
                       [ selem "ele"   [ txt (show $ rteEle x) ]
                       , selem "time"  [ txt (rteTim x) ]
                       , selem "name"  [ txt (rteNam x) ]
                       ]
                ) rts
