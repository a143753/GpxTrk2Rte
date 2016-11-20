module Lib
    ( plotTree
    , getTrkName
    , getTrkSeg
    , tplToTrkPt
    , trkPtToRtePt
    , writeRte
    , angle
    , metric
    , deci
    ) where

import Text.XML.HXT.Core
import Data.List
import Debug.Trace
--import Text.XML.HXT.Curl

-- (lat,lon,ele,time)
data TrkPt = TrkPt Double Double Double deriving(Show)
trkLat (TrkPt lat _ _) = lat
trkLon (TrkPt _ lon _) = lon
trkEle (TrkPt _ _ ele) = ele

-- (name, lat,lon,ele)
data RtePt = RtePt String Double Double Double deriving(Show)
rteNam (RtePt nam _ _ _) = nam
rteLat (RtePt _ lat _ _) = lat
rteLon (RtePt _ _ lon _) = lon
rteEle (RtePt _ _ _ ele) = ele

-- function to print XML Document in tree format.
plotTree :: FilePath -> FilePath -> IOSArrow XmlTree XmlTree
plotTree infile outfile = 
    readDocument [withValidate no] infile >>>
    putXmlTree outfile

-- function to convert tuples to TrkPt
tplToTrkPt :: (String,(String,String)) -> TrkPt
tplToTrkPt (a,(b,c)) = TrkPt (read a) (read b) (read c)

-- function to convert TrkPt to RtePt
trkPtToRtePt :: [TrkPt] -> [RtePt]
trkPtToRtePt tp = map f $ zip [1..] tp
    where
      f (n,p) = RtePt ("No. "++show(n)) (trkLat p) (trkLon p) (trkEle p)

-- -> (lat,(lon,(ele,(time))))
getTrkName :: IOSArrow XmlTree String
getTrkName =
    getChildren >>>
    isElem >>> hasName "gpx" >>>
    getChildren >>>
    isElem >>> hasName "trk" >>>
    getChildren >>>
    isElem >>> hasName "name" >>> 
    getChildren >>>
    getText

-- -> (lat,(lon,(ele)))
getTrkSeg :: IOSArrow XmlTree ( String, (String, String))
getTrkSeg =
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
    ( getChildren >>> isElem >>> hasName "ele"  >>> getChildren >>> getText )

-- make RTE document tree
writeRte :: ArrowXml a => String -> [RtePt] -> a XmlTree XmlTree
writeRte name rts = mkelem "gpx"
               [ sattr "version" "1.1"
               , sattr "creator" "GpxTrk2Rte"
               , sattr "xmlns"   "http://www.topografix.com/GPX/1/1"
               , sattr "xsi:schemaLocation" "http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd http://www.kashmir3d.com/namespace/kashmir3d http://www.kashmir3d.com/namespace/kashmir3d.xsd"
               ]
               [ selem "rte"
                 ([ selem "name"   [ txt name ]
                  , selem "number" [ txt "1" ]
                  ] ++ pts)
               ]
    where
      pts = map (\x -> mkelem "rtept"
                       [ sattr "lat"   (show $ rteLat x)
                       , sattr "lon"   (show $ rteLon x)
                       ]
                       [ selem "ele"   [ txt (show $ rteEle x) ]
                       , selem "name"  [ txt (rteNam x) ]
                       ]
                ) rts

-- angle 
angle :: (Double,Double) -> (Double,Double) -> Double
angle (ax,ay) (bx,by) = ( (ax*bx + ay*by) / ( sqrt(ax*ax+ay*ay) * sqrt(bx*bx+by*by) ) )

-- angle' : reciprocal of inner product
angle' :: (Double,Double) -> (Double,Double) -> Double
angle' (ax,ay) (bx,by) = 1.0 / (ax*bx + ay*by)

-- metric : calculate nodes' importance. smaller the metric, less important 
metric :: [RtePt] -> [(String,Double)]
metric (p0:p1:p2:ps) =
    let a = ( (rteLon p0) - (rteLon p1), (rteLat p0) - (rteLat p1) )
        b = ( (rteLon p2) - (rteLon p1), (rteLat p2) - (rteLat p1) )
        n = rteNam p1
        cos0 = angle a b
    in
      (n,cos0) : metric(ps)
metric (p0:ps) = []
metric [] = []

-- take'
take' :: [(String,Double)] -> RtePt -> Bool
take' [] _ = False
take' (m:ms) a
    | (fst m) == (rteNam a)  = True
    | otherwise = take' ms a

drop' :: (String,Double) -> [RtePt] -> [RtePt]
drop' a (m:ms)
      | (fst a) == (rteNam m) = ms
      | otherwise             = m:(drop' a ms)
drop' a [] = []

-- decimation
-- it is easy to process rtept because each node has the name.
deci :: Int -> [RtePt] -> [RtePt]
deci n x
    | n >= length x = x  -- no need to decimate
    | otherwise     =
        let
          len = length x
          met = minimumBy (\x y -> compare (snd x) (snd y) ) $ metric x
        in
          deci n $ drop' met x
