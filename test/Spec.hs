import Test.HUnit
import System.IO
import Lib

assertFloatEqual text a b = 
    assertEqual text (take 10 (show a)) (take 10 (show b))

tests = TestList
        [
         "angle1" ~: angle ( 1, 0) ( 0, 1) ~?= 0.0
        ,"angle2" ~: angle ( 1, 0) ( 1, 0) ~?= 1.0
        ,"angle3" ~: angle (-1, 0) (-1, 0) ~?= 1.0
        ,TestLabel "angle4" $ TestCase $ assertFloatEqual "" sqrt(0.5) (angle(1,1) (0,1))
        ]
   
--main :: IO ()
main = do
  runTestText (putTextToHandle stderr False) tests
