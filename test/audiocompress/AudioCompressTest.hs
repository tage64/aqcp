import AudioCompress as AC
import Samples
import Data.Vector.Storable as V
import Test.HUnit

{- Variables for testing
-}
smallList :: SampleList
smallList = fromList [1..10]

largeList :: SampleList
largeList = fromList [1..32000]

emptyList :: SampleList
emptyList = fromList []

{- main function for testing. Use stack test.
-}
main :: IO ()
main = do
 
  assertEqual "Test small samplelist" (decompress (compress smallList)) smallList
  assertEqual "Testing large samplelist" (decompress (compress largeList)) largeList
  -- Tests if an empty samplelist is in fact an empty Data.Vector
  assertEqual "Testing empty samplelist" (decompress (compress emptyList)) V.empty
