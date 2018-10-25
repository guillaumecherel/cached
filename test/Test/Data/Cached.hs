{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Data.Cached where

import Protolude

import Data.Text
import Data.Text.Read
import Development.Shake
import Development.Shake.FilePath
import System.Directory
import Test.QuickCheck
import Test.Util

import Data.Cached as Cached

readInt :: Text -> Either Text Int
readInt = bimap pack fst . signed decimal

val :: Int -> FilePath -> Cached Int
val x p = pure x
          & cache p show readInt

addOne :: FilePath -> Cached (Int -> Int)
addOne p = pure (+ 1)

shakeGo testDir = shakeArgs shakeOptions
  { shakeReport = [testDir </> "shakeReport.html"]
  , shakeFiles = testDir </> ".shake/"
  }

-- Make sure the given file does not exist, delete if necessary.
rmFile :: FilePath -> IO ()
rmFile f = do
    System.Directory.doesFileExist f
    >>= bool (return ()) (removeFile f)

-- Make sure the given directory does not exist, delete recursively if necessary.
rmRec:: FilePath -> IO ()
rmRec dir = removePathForcibly dir

-- Test that a file exists and contains the appropriate value.
testFile :: FilePath -> Text -> IO Property
testFile path content = do
  exists <- System.Directory.doesFileExist path
  if not exists
    then return $ counterexample ("File " <> path <> " does not exist") False
    else do 
      valInFile <- readFile path
      return $ counterexample ("Cached file " <> path <> " contains "
                                <> show valInFile
                                <> " but should contain \""
                                <> unpack content
                                <> "\".")
                              (valInFile == content)

-- Run build in a clean directory
runClean :: FilePath -> Cached a -> IO ()
runClean dir x = do
  rmRec dir
  shakeGo dir $ buildCache x

  
-- Test function application.
testFunAp :: FilePath -> IO () -> (Text, Text) ->  Property
testFunAp dir touch (expectedA, expectedFA) = once $ ioProperty $ do
  let a x = pure x
          & cache (dir </> "a") show readInt
  let f i a = pure (+ (10 * i)) <*> a
           & cache (dir </> "fa") show readInt

  -- Delete shake cache files
  rmRec dir

  -- Build "a" once
  shakeGo dir $ do
    buildCache $ f 1 (a 1)

  touch

  -- Build cache
  shakeGo dir $ do
    buildCache $ f 2 (a 2)

  -- Test the cache files
  testA <- testFile (dir </> "a") expectedA
  testFA <- testFile (dir </> "fa") expectedFA

  return $ testA .&&. testFA



---- Tests ----

---- Single values depending on no other cached value

-- Check that when a value is computed and there is no cache file, it is created.
prop_SinValCreaCache :: Property
prop_SinValCreaCache = ioProperty $ do
  let a = val 1 "test-output/formulas/Util/Cached/SinValCreaCache/a"

  -- Make sure the directory state is clean
  rmRec "test-output/formulas/Util/Cached/SinValCreaCache"

  -- Build the cache
  shakeGo "test-output/formulas/Util/Cached/SinValCreaCache/" $ buildCache a

  -- Test if the file was written and contains the appropriate value
  testFile "test-output/formulas/Util/Cached/SinValCreaCache/a" "1"
 

-- Check that a value is not recomputed when the cache file already exists
prop_SinValNoRecomp :: Property
prop_SinValNoRecomp = ioProperty $ do
  let a x = val x "test-output/formulas/Util/Cached/SinValNoRecomp/a"

  -- Make sure the file is not present
  rmRec "test-output/formulas/Util/Cached/SinValNoRecomp"

  -- Build the cache once.
  shakeGo "test-output/formulas/Util/Cached/SinValNoRecomp" $ buildCache (a 1)

  -- Rebuild, changing the value.
  shakeGo "test-output/formulas/Util/Cached/SinValNoRecomp" $ buildCache (a 2)

  -- Test if the file was written and contains the appropriate value
  testFile "test-output/formulas/Util/Cached/SinValNoRecomp/a" "1"


-- Check that a value is recomputed when the cache file has changed.
prop_SinValRecomp :: Property
prop_SinValRecomp = ioProperty $ do
  let a x = val x "test-output/formulas/Util/Cached/SinValRecomp/a"

  -- Make sure the file is not present
  rmRec  "test-output/formulas/Util/Cached/SinValRecomp"

  -- Build the cache once.
  shakeGo "test-output/formulas/Util/Cached/SinValRecomp" $ buildCache (a 1)

  -- Modify the cache file. (delay to make sure the file modification date is different)
  threadDelay 5000
  writeFile "test-output/formulas/Util/Cached/SinValRecomp/a" "2"

  -- Rebuild, changing the value.
  shakeGo "test-output/formulas/Util/Cached/SinValRecomp/" $ buildCache (a 3)

  -- Test if the file was written and contains the appropriate value
  testFile "test-output/formulas/Util/Cached/SinValRecomp/a" "3"


---- Function application (cached values depending on another cached value)

-- State: A absent, FA absent
-- Check that when no cache file is present, the function application `pure f <*> a` creates a cache file for `a` and for the last value `f <$> a`.
prop_FunApAAbsentFAAbsent :: Property
prop_FunApAAbsentFAAbsent =
  testFunAp "test-output/formulas/Util/Cached/FunApAAbsentFAAbsent"
            (do
              rmFile "test-output/formulas/Util/Cached/FunApAAbsentFAAbsent/a"
              rmFile "test-output/formulas/Util/Cached/FunApAAbsentFAAbsent/fa")
            ("2", "22")

-- State: A unchanged, FA absent
-- Check that when the cache file for `a` only already exists, a is not recomputed and the cache file for `f <$> a` is created.
prop_FunApAUnchangedFAAbsent :: Property
prop_FunApAUnchangedFAAbsent =
  testFunAp "test-output/formulas/Util/Cached/FunApAUnchangedFAAbsent"
            (do
              rmFile "test-output/formulas/Util/Cached/FunApAUnchangedFAAbsent/fa")
            ("1", "21")

-- State: A modified, FA absent
-- Check that a is recomputed and FA is recomputed
prop_FunApAModifiedFAAbsent :: Property
prop_FunApAModifiedFAAbsent =
  testFunAp "test-output/formulas/Util/Cached/FunApAModifiedFAAbsent"
            (do
              threadDelay 5000
              writeFile "test-output/formulas/Util/Cached/FunApAModifiedFAAbsent/a" "3"
              rmFile "test-output/formulas/Util/Cached/FunApAModifiedFAAbsent/fa")
            ("2", "22")




-- State: A absent, FA unchanged
-- Check that "a" and "fa" are recomputed
prop_FunApAAbsentFAUnchanged :: Property
prop_FunApAAbsentFAUnchanged =
  testFunAp "test-output/formulas/Util/Cached/FunApAAbsentFAUnchanged"
            (do
              threadDelay 5000
              rmFile "test-output/formulas/Util/Cached/FunApAAbsentFAUnchanged/a")
            ("2", "22")

-- State: A unchanged, FA unchanged
-- Check that nothing is recomputed
prop_FunApAUnchangedFAUnchanged :: Property
prop_FunApAUnchangedFAUnchanged =
  testFunAp "test-output/formulas/Util/Cached/FunApAUnchangedFAUnchanged"
            (do
              return ())
            ("1", "11")

-- State: A modified, FA unchanged
-- Check that a is recomputed and FA is recomputed
prop_FunApAModifiedFAUnchanged :: Property
prop_FunApAModifiedFAUnchanged =
  testFunAp "test-output/formulas/Util/Cached/FunApAModifiedFAUnchanged"
            (do
              threadDelay 5000
              writeFile "test-output/formulas/Util/Cached/FunApAModifiedFAUnchanged/a" "3")
            ("2", "22")




-- State: A absent, FA modified
-- Check that both are recomputed
prop_FunApAAbsentFAModified :: Property
prop_FunApAAbsentFAModified =
  testFunAp "test-output/formulas/Util/Cached/FunApAAbsentFAModified"
            (do
              rmFile "test-output/formulas/Util/Cached/FunApAAbsentFAModified/a"
              threadDelay 5000
              writeFile "test-output/formulas/Util/Cached/FunApAAbsentFAModified/fa" "77")
            ("2", "22")

-- State: A unchanged, FA modified
-- Check that "fa" is recomputed but not "a" 
prop_FunApAUnchangedFAModified :: Property
prop_FunApAUnchangedFAModified =
  testFunAp "test-output/formulas/Util/Cached/FunApAUnchangedFAModified"
            (do
              threadDelay 5000
              writeFile "test-output/formulas/Util/Cached/FunApAUnchangedFAModified/fa" "77")
            ("1", "21")

-- State: A modified, FA modified
-- Check that both are recomputed.
prop_FunApAModifiedFAModified :: Property
prop_FunApAModifiedFAModified =
  testFunAp "test-output/formulas/Util/Cached/FunApAModifiedFAModified"
            (do
              threadDelay 5000
              writeFile "test-output/formulas/Util/Cached/FunApAModifiedFAModified/a" "3"
              threadDelay 5000
              writeFile "test-output/formulas/Util/Cached/FunApAModifiedFAModified/fa" "77")
            ("2", "22")


-- State: A absent, FA absent
-- Check with the Functor operator "f <$> a"
prop_FunApCompAllFunctor :: Property
prop_FunApCompAllFunctor = once $ ioProperty $ do
  let a = pure 1
          & cache "test-output/formulas/Util/Cached/FunApCompAllFunctor/a" show readInt
  let fa = pure (+ 1) <*> a
           & cache "test-output/formulas/Util/Cached/FunApCompAllFunctor/fa" show readInt

  -- Delete shake cache files
  rmRec "test-output/formulas/Util/Cached/FunApCompAllFunctor"

  -- Build cache
  shakeGo "test-output/formulas/Util/Cached/FunApCompAllFunctor/" $ do
    buildCache fa

  -- Test the cache files
  testA <- testFile "test-output/formulas/Util/Cached/FunApCompAllFunctor/a" "1"
  testFA <- testFile "test-output/formulas/Util/Cached/FunApCompAllFunctor/fa" "2"

  return $ testA .&&. testFA


---- Monoid

-- The monoid is left-biased: when two different values are cached to the same
-- target, the first is kept.
prop_MonoidCacheTwice :: Property
prop_MonoidCacheTwice = ioProperty $ do
  let mon = cache "test-output/formulas/Util/Cached/Monoid/CacheTwice/a"
                  (show . getSum) (fmap Sum . readInt) (pure (Sum 1))
         <> cache "test-output/formulas/Util/Cached/Monoid/CacheTwice/a"
                  (show . getSum) (fmap Sum . readInt) (pure (Sum 2))
  runClean "test-output/formulas/Util/Cached/Monoid/CacheTwice" mon
  testFile "test-output/formulas/Util/Cached/Monoid/CacheTwice/a" "1"


---- Applicative

-- The applicative is left-biased: Check that caching twice to the same file
-- only builds the first.
prop_ApplicativeCacheTwice :: Property
prop_ApplicativeCacheTwice = ioProperty $ do
  let ap = cache "test-output/formulas/Util/Cached/Applicative/CacheTwice/a"
                  (\_ -> "1") (\_ -> Right (+ 1)) (pure (+ 1))
       <*> cache "test-output/formulas/Util/Cached/Applicative/CacheTwice/a"
                  (\_ -> "2") (\_ -> Right 1) (pure 1)
                  
  runClean "test-output/formulas/Util/Cached/Applicative/CacheTwice" ap
  testFile "test-output/formulas/Util/Cached/Applicative/CacheTwice/a" "1"



runTests = do
  checkOrExit "prop_SinValCreaCache" prop_SinValCreaCache
  checkOrExit "prop_SinValNoRecomp" prop_SinValNoRecomp
  checkOrExit "prop_SinValRecomp" prop_SinValRecomp
  checkOrExit "prop_FunApAAbsentFAAbsent" prop_FunApAAbsentFAAbsent
  checkOrExit "prop_FunApAAbsentFAUnchanged" prop_FunApAAbsentFAUnchanged
  checkOrExit "prop_FunApAAbsentFAModified" prop_FunApAAbsentFAModified
  checkOrExit "prop_FunApAUnchangedFAAbsent" prop_FunApAUnchangedFAAbsent
  checkOrExit "prop_FunApAUnchangedFAUnchanged" prop_FunApAUnchangedFAUnchanged
  checkOrExit "prop_FunApAUnchangedFAModified" prop_FunApAUnchangedFAModified
  checkOrExit "prop_FunApAModifiedFAAbsent" prop_FunApAModifiedFAAbsent
  checkOrExit "prop_FunApAModifiedFAUnchanged" prop_FunApAModifiedFAUnchanged
  checkOrExit "prop_FunApAModifiedFAModified" prop_FunApAModifiedFAModified
  checkOrExit "prop_FunApCompAllFunctor" prop_FunApCompAllFunctor
  checkOrExit "prop_MonoidCacheTwice" prop_MonoidCacheTwice
  checkOrExit "prop_ApplicativeCacheTwice" prop_ApplicativeCacheTwice

