import Test.DocTest
import Test.Data.Cached (runTests)

main :: IO ()
main = do
  putStrLn "---- QuickCheck"
  runTests
  putStrLn "---- DocTest"
  doctest [ "src/Data/Cached.hs"
          , "src/Data/Cached/Internal.hs"]
