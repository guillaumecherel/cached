{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Test.Util where

import Protolude
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (unpack, pack)
import System.Directory
import System.FilePath
import Test.QuickCheck
import Test.QuickCheck.Assertions
import Test.QuickCheck.Test

import Data.Cached
import Data.Cached.Internal

checkOrExit :: (Testable prop) => Text -> prop -> IO ()
checkOrExit msg p = do
  success <- fmap isSuccess $ quickCheckResult
             $ label (unpack msg)
             $ counterexample ("Failing property: " ++ unpack msg)
             p
  when (not success) exitFailure

-- Make sure the given file does not exist, delete if necessary.
rmFile :: FilePath -> IO ()
rmFile f = do
    System.Directory.doesFileExist f
    >>= bool (return ()) (removeFile f)

-- Make sure the given directory does not exist, delete recursively if necessary.
rmRec:: FilePath -> IO ()
rmRec dir = removePathForcibly dir

-- Set a directory to contain only the given files and their content
setDir :: FilePath -> [(FilePath, Text)] -> IO ()
setDir rootDir content = do
  rmRec rootDir
  createDirectory rootDir
  foldMap (\(p, t) -> do
            createDirectoryIfMissing True (takeDirectory p)
            writeFile p t)
          content

-- Turn a cache into a value that can be inspected. This function
-- runs the IO actions that are contained in the cache (cachedValue
-- and Build), and expects that the IO actions only read and write files,
-- and that the IO actions in the build only writes to files that are
-- specified as targets (the Build keys).
materialize :: Cached a
            -> IO (Either Text a, Set FilePath,
                   Map FilePath (Either Text Text, Set FilePath))
materialize a = do
  let needs = cacheNeeds a
  let runAndReadBack f ioe = ioe >>= \e -> readFile f >>= \r -> return (r <$ e)
  build <- Map.traverseWithKey
             (\k (ac, n) -> runAndReadBack k (runExceptT ac)
                        >>= \r -> return (r,n))
             (getBuild $ cacheBuild a)
  val <- getValue a
  return (val, needs, build)

-- Test the value, needs, dependencies of a given cached value, and the content written to files.
testCached :: (Eq a, Show a)
          => Cached a
          -> Either Text a
          -> Set FilePath
          -> Map FilePath (Either Text Text, Set FilePath)
          -> IO Property
testCached c value needs build = do
  (val', needs', build') <- materialize c
  return $ value ==? val'
      .&&. needs ==? needs'
      .&&. build ==? build'


-- Test equality of two cached values: values, needs, and builds. Runs all IO actions in the cache.
eqCached :: (Eq a, Show a) => Cached a -> Cached a -> IO Property
eqCached a b = property <$> ((==?) <$> materialize a <*> materialize b)

newtype Seed = Seed Int deriving (Show)

instance Arbitrary Seed where
  arbitrary = Seed <$> choose (-100000, 100000)

newtype SimpleFileName = SimpleFileName {getSimpleFileName :: FilePath}
  deriving (Eq, Show, Ord)

instance Arbitrary SimpleFileName where
  arbitrary = scale (min 10)
            $ SimpleFileName
          <$> genStr `suchThat` (not . flip elem [".", ".."])
    where genStr :: Gen FilePath
          genStr = listOf1 $ elements
                 $ (['a'..'z'] ++ ['A'..'Z'] ++ ['_', '-', '.'])

newtype SimplePath = SimplePath {getSimplePath :: FilePath}
  deriving (Show, Ord, Eq)

instance Arbitrary SimplePath where
  arbitrary = SimplePath . intercalate "/"
          <$> scale (min 10)
              (listOf1 $ fmap getSimpleFileName arbitrary )

newtype DirectoryTree = DT (Map SimpleFileName DirectoryTree)
  deriving (Eq, Show, Ord)

instance Arbitrary DirectoryTree where
  arbitrary = sized genDirectoryTree

genDirectoryTree :: Int -> Gen DirectoryTree
genDirectoryTree 0 = pure $ DT mempty
genDirectoryTree 1 = (\f -> DT $ Map.singleton f (DT mempty))
                 <$> arbitrary
genDirectoryTree n = do
  nTop <- choose (1, n)
  splits <- List.sort <$> vectorOf (nTop - 1) (choose (0,n))
  let nChildren = fmap (\(a, b) -> b - a)
                     (zip (0:splits) (splits ++ [n]))
  filenames <- vector nTop
  subTrees <- traverse genDirectoryTree nChildren
  return $ DT $ Map.fromList $ zip filenames subTrees

directoryTreePaths :: DirectoryTree -> [FilePath]
directoryTreePaths (DT m) = do
  (SimpleFileName f, DT d) <- Map.toList m
  if null d
    then [f]
    else fmap ((f <> "/") <>) (directoryTreePaths (DT d))

instance Arbitrary Text where
  arbitrary = pack <$> (arbitrary :: Gen [Char])

genCached :: [FilePath] -> Gen (Cached Int)
genCached [] = genCachedPure
genCached paths = sized $ \s ->
  if s == 0
    then genCachedPure
    else oneof
      [ genCachedPure
      , genCachedFile paths
      , genCachedFunctor paths
      , genCachedApplicative paths ]

-- | Generate a pure cached value that is not associated to a cache file
genCachedPure :: (Arbitrary a) => Gen (Cached a)
genCachedPure = fmap pure arbitrary

-- | Generate a cached value that is associated to a cache file
genCachedFile :: [FilePath] -> Gen (Cached Int)
genCachedFile (path:paths) = sized $ \s ->
  fmap (cache' path) (resize (s - 1) $ genCached paths)

-- | Generate a cached value that is the result of an fmap.
genCachedFunctor :: [FilePath] -> Gen (Cached Int)
genCachedFunctor paths = sized $ \s -> do
  f <- arbitrary
  c <- resize (s - 1) $ genCached paths
  return $ fmap f c
 
-- | Generate a cached value that is the result of combining two cached values
-- together with <*>.
genCachedApplicative :: [FilePath] -> Gen (Cached Int)
genCachedApplicative paths = sized $ \s -> do
  f <- arbitrary
  split <- choose (0, s - 1)
  c1 <- resize (split) $ genCached paths1
  c2 <- resize (s - 1 - split) $ genCached paths2
  return $ f <$> c1 <*> c2
  where (paths1, paths2) = splitRL paths ([],[])
        splitRL [] (p1, p2) = (p1, p2)
        splitRL (p:ps) (p1, p2) = splitRL ps (p:p2, p1)

