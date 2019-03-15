{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Data.Cached where

import Protolude

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text
import Data.Text.Read
import Development.Shake
import Development.Shake.FilePath
import System.Directory
import Test.QuickCheck
import Test.QuickCheck.Assertions
import Test.Util

import Data.Cached
import Data.Cached.Internal

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
  shakeGo dir $ toShakeRules x

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
    toShakeRules $ f 1 (a 1)

  touch

  -- Build cache
  shakeGo dir $ do
    toShakeRules $ f 2 (a 2)

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
  shakeGo "test-output/formulas/Util/Cached/SinValCreaCache/" $ toShakeRules a

  -- Test if the file was written and contains the appropriate value
  testFile "test-output/formulas/Util/Cached/SinValCreaCache/a" "1"
 

-- Check that a value is not recomputed when the cache file already exists
prop_SinValNoRecomp :: Property
prop_SinValNoRecomp = ioProperty $ do
  let a x = val x "test-output/formulas/Util/Cached/SinValNoRecomp/a"

  -- Make sure the file is not present
  rmRec "test-output/formulas/Util/Cached/SinValNoRecomp"

  -- Build the cache once.
  shakeGo "test-output/formulas/Util/Cached/SinValNoRecomp" $ toShakeRules (a 1)

  -- Rebuild, changing the value.
  shakeGo "test-output/formulas/Util/Cached/SinValNoRecomp" $ toShakeRules (a 2)

  -- Test if the file was written and contains the appropriate value
  testFile "test-output/formulas/Util/Cached/SinValNoRecomp/a" "1"


-- Check that a value is recomputed when the cache file has changed.
prop_SinValRecomp :: Property
prop_SinValRecomp = ioProperty $ do
  let a x = val x "test-output/formulas/Util/Cached/SinValRecomp/a"

  -- Make sure the file is not present
  rmRec  "test-output/formulas/Util/Cached/SinValRecomp"

  -- Build the cache once.
  shakeGo "test-output/formulas/Util/Cached/SinValRecomp" $ toShakeRules (a 1)

  -- Modify the cache file. (delay to make sure the file modification date is different)
  threadDelay 5000
  writeFile "test-output/formulas/Util/Cached/SinValRecomp/a" "2"

  -- Rebuild, changing the value.
  shakeGo "test-output/formulas/Util/Cached/SinValRecomp/" $ toShakeRules (a 3)

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
    toShakeRules fa

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


---- Interface functions examples

prop_source :: SimplePath -> Either Text Int -> Property
prop_source (SimplePath p) es =
  let dir="test-output/formulas/Util/Cached/Interface/source"
      path = dir </> p
      c = source path (bimap pack identity . readEither . unpack)
      -- to cover the case when es is (Left t) and t represents an Int
      es' = case es of
                      Left t -> case readMaybe (unpack t) of
                                  Nothing -> es
                                  Just anInt -> Right anInt
                      _ -> es
  in ioProperty $ do
    setDir dir [(path, case es of Right i -> show i; Left t -> t)]
    ev <- getValue c
    return $ counterexample (show (ev, es')) $ case (ev, es') of
      (Left errv, Left _ ) -> errv == "Error reading file " <> pack path <> ": Prelude.read: no parse"
      (Right v, Right s) -> v == s
      _ -> False

prop_source' :: SimplePath -> Either Text Int -> Property
prop_source' (SimplePath p) es =
  let dir="test-output/formulas/Util/Cached/Interface/source'"
      path = dir </> p
  in ioProperty $ do
    setDir dir [(path, case es of Right i -> show i; Left t -> t)]
    eqCached (source' path :: Cached Int)
             (source path (bimap pack identity . readEither . unpack) :: Cached Int)

prop_fromIO :: DirectoryTree -> InfiniteList Text -> Property
prop_fromIO dt (InfiniteList infTexts _) =
  let dir="test-output/formulas/Util/Cached/Interface/fromIO"
      paths = fmap (dir </>) (directoryTreePaths dt)
      texts = List.take (List.length paths) infTexts
      c = fromIO (Set.fromList paths) (foldMap readFile paths)
  in ioProperty $ do
    setDir dir (Protolude.zip paths texts)
    e <- getValue $ liftA2 (==) c (pure $ mconcat texts)
    return $ case e of
      Left _ -> False
      Right b -> b

prop_cache :: Property
prop_cache = once $
  let dir = "test-output/formulas/Util/Cached/Interface/cache"
      c1 = cache ( dir </> "c1" )
                 show
                 (bimap pack identity . readEither . unpack)
                 ( pure 1 ) :: Cached Int
      c2 = cache ( dir </> "c2" ) 
                 show
                 (bimap pack identity . readEither . unpack)
                 ( pure 2 ) :: Cached Int
      c3 = cache ( dir </> "c3" ) 
                 show
                 (bimap pack identity . readEither . unpack)
                 ( c1 + c2 ) :: Cached Int
  in ioProperty $ do
    setDir dir []
    test1 <- testCached c1 (Right 1) (Set.singleton $ dir </> "c1")
               (Map.fromList [(dir </> "c1", (Right "1", mempty))])
    test2 <- testCached c3 (Right 3) (Set.singleton $ dir </> "c3")
               (Map.fromList [(dir </> "c3", (Right "3", Set.fromList [dir </> "c1"
                                                         ,dir </> "c2"]))
                             ,(dir </> "c2", (Right "2", mempty))
                             ,(dir </> "c1", (Right "1", mempty))])
    return $ test1 .&&. test2

prop_cache' :: Property
prop_cache' = once $
  let dir = "test-output/formulas/Util/Cached/Interface/cache'"
      c1 = cache' ( dir </> "c1" ) ( pure 1 ) :: Cached Int
      c2 = cache' ( dir </> "c2" ) ( pure 2 ) :: Cached Int
      c3 = cache' ( dir </> "c3" ) ( c1 + c2 ) :: Cached Int
  in ioProperty $ do
    setDir dir []
    test1 <- testCached c1 (Right 1) (Set.singleton $ dir </> "c1")
               (Map.fromList [(dir </> "c1", (Right "1", mempty))])
    test2 <- testCached c3 (Right 3) (Set.singleton $ dir </> "c3")
               (Map.fromList [(dir </> "c3", (Right "3", Set.fromList [dir </> "c1"
                                                         ,dir </> "c2"]))
                             ,(dir </> "c2", (Right "2", mempty))
                             ,(dir </> "c1", (Right "1", mempty))])
    return $ test1 .&&. test2


prop_cacheIO :: Property
prop_cacheIO = once $
  let dir = "test-output/formulas/Util/Cached/Interface/cacheIO"
      c1 = cacheIO ( dir </> "c1" )
                  ( writeFile (dir </> "c1") . show )
                  ( bimap pack identity . readEither . unpack
                    <$> readFile (dir </> "c1") )
                  ( pure 1 ) :: Cached Int
      c2 = cacheIO ( dir </> "c2" )
                  ( writeFile (dir </> "c2") . show )
                  ( bimap pack identity . readEither . unpack
                    <$> readFile (dir </> "c2") )
                  ( pure 2 ) :: Cached Int
      c3 = cacheIO ( dir </> "c3" )
                  ( writeFile (dir </> "c3") . show )
                  ( bimap pack identity . readEither . unpack
                    <$> readFile (dir </> "c3") )
                  ( c1 + c2 ) :: Cached Int
  in ioProperty $ do
    setDir dir []
    test1 <- testCached c1 (Right 1) (Set.singleton $ dir </> "c1")
               (Map.fromList [(dir </> "c1", (Right "1", mempty))])
    test2 <- testCached c3 (Right 3) (Set.singleton $ dir </> "c3")
               (Map.fromList [(dir </> "c3", (Right "3", Set.fromList [dir </> "c1"
                                                         ,dir </> "c2"]))
                             ,(dir </> "c2", (Right "2", mempty))
                             ,(dir </> "c1", (Right "1", mempty))])
    return $ test1 .&&. test2

prop_sink :: Property
prop_sink = once $
  let dir = "test-output/formulas/Util/Cached/Interface/sink"
      c1 = cache' ( dir </> "c1" ) ( pure 1 ) :: Cached Int
      c2 = cache' ( dir </> "c2" ) ( pure 2 ) :: Cached Int
      s1 = sink ( dir </> "s1" ) show ( pure (1 :: Int) )
      s2 = sink ( dir </> "s2" ) show ( c1 + c2 )
  in ioProperty $ do
    setDir dir []
    test1 <- testCached s1 (Right ()) mempty
               (Map.fromList [(dir </> "s1", (Right "1", mempty))])
    test2 <- testCached s2 (Right ()) mempty
               (Map.fromList [(dir </> "s2", (Right "3", Set.fromList [dir </> "c1"
                                                         ,dir </> "c2"]))
                             ,(dir </> "c2", (Right "2", mempty))
                             ,(dir </> "c1", (Right "1", mempty))])
    return $ test1 .&&. test2

prop_sink' :: Property
prop_sink' = once $
  let dir = "test-output/formulas/Util/Cached/Interface/sink'"
      c1 = cache' ( dir </> "c1" ) ( pure 1 ) :: Cached Int
      c2 = cache' ( dir </> "c2" ) ( pure 2 ) :: Cached Int
      s1 = sink' ( dir </> "s1" ) (pure 1):: Cached ()
      s2 = sink' ( dir </> "s2" ) (c1 + c2) :: Cached ()
  in ioProperty $ do
    setDir dir []
    test1 <- testCached s1 (Right ()) mempty
               (Map.fromList [(dir </> "s1", (Right "1", mempty))])
    test2 <- testCached s2 (Right ()) mempty
               (Map.fromList [(dir </> "s2", (Right "3", Set.fromList [dir </> "c1"
                                                         ,dir </> "c2"]))
                             ,(dir </> "c2", (Right "2", mempty))
                             ,(dir </> "c1", (Right "1", mempty))])
    return $ test1 .&&. test2

prop_sinkIO :: Property
prop_sinkIO = once $
  let dir = "test-output/formulas/Util/Cached/Interface/sinkIO"
      c1 = cache' ( dir </> "c1" ) ( pure 1 ) :: Cached Int
      c2 = cache' ( dir </> "c2" ) ( pure 2 ) :: Cached Int
      s1 = sinkIO ( dir </> "s1" )
                  ( writeFile (dir </> "s1") . show )
                  (pure 1):: Cached ()
      s2 = sinkIO ( dir </> "s2" )
                  ( writeFile (dir </> "s2") . show )
                  (c1 + c2) :: Cached ()
  in ioProperty $ do
    setDir dir []
    test1 <- testCached s1 (Right ()) mempty
               (Map.fromList [(dir </> "s1", (Right "1", mempty))])
    test2 <- testCached s2 (Right ()) mempty
               (Map.fromList [(dir </> "s2", (Right "3", Set.fromList [dir </> "c1"
                                                         ,dir </> "c2"]))
                             ,(dir </> "c2", (Right "2", mempty))
                             ,(dir </> "c1", (Right "1", mempty))])
    return $ test1 .&&. test2

prop_trigger :: Property
prop_trigger = once $
  let dir = "test-output/formulas/Util/Cached/Interface/trigger"
      c = trigger ( dir </> "c" )
                  ( writeFile (dir </> "c") "1" )
                  ( Set.fromList [dir </> "a"] )
  in ioProperty $ do
    setDir dir []
    testCached c (Right ()) mempty
               (Map.fromList [(dir </> "s1", (Right "1", (Set.fromList [dir </> "a"])))])

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
  checkOrExit "prop_source" prop_source
  checkOrExit "prop_source'" prop_source'
  checkOrExit "prop_fromIO" prop_fromIO
  checkOrExit "prop_cache" prop_cache
  checkOrExit "prop_cache'" prop_cache'
  checkOrExit "prop_cacheIO" prop_cacheIO
  checkOrExit "prop_sink" prop_sink
  checkOrExit "prop_sink'" prop_sink'
  checkOrExit "prop_sinkIO" prop_sinkIO
  checkOrExit "prop_trigger" prop_trigger

