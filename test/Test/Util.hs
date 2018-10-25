module Test.Util where

import Protolude
import Data.Text (unpack)
import Test.QuickCheck
import Test.QuickCheck.Test

newtype Seed = Seed Int deriving (Show)

instance Arbitrary Seed where
  arbitrary = Seed <$> choose (-100000, 100000)

checkOrExit :: (Testable prop) => Text -> prop -> IO ()
checkOrExit msg p = do
  success <- fmap isSuccess $ quickCheckResult
             $ label (unpack msg)
             $ counterexample ("Failing property: " ++ unpack msg)
             p
  when (not success) exitFailure
