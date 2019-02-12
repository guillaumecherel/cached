{-| This module lets you cache values to disk to avoid re-running (potentially
    long) computations between consecutive executions of your
    program. Cached values are recomputed only when needed, i.e. when
    other cached values on which they depend change. Independent
    computations are run in parallel. It offers convenient
    fonctions for caching to text files, but caching and uncaching using
    arbitrary IO actions is also possible.

    The module was motivated by writing scientific data flows, simulation
    experiments or data science scripts. Those often involve long
    computations and create "flows" where the output of some computation
    are the inputs of others, until final results are produced (values,
    figures, statistical tests, etc.).

    = Usage

    A value of type "Cached a" should be understood as a value of type "a" that is read from a file, or that is produced from data stored in one or more files, or from arbitrary IO actions.

    Cached values can be created from pure values,

>>> let a = cache' "/tmp/cached-ex/a" (pure 1) :: Cached Int

    or from files stored on disk.

>>> let b = source' "/tmp/cached-ex/b" :: Cached Int

    Use the functor and applicative instances to use cached values in
    new computations.

>>> let bigA = fmap (* 2) a
>>> let c = (+) <$> a <*> b

    The cached value "c" represents a value produced by summing
    together the values stored in "\/tmp\/cached-ex\/a" and
    "\/tmp\/cached-ex\/b". It is not yet associated to its own cache
    file. To actually store it into a cache file, use 'cache'' (or 'cache' for values that are not instances of Show and
    Read or when you want to control the file format, e.g. writing arrays
    to CSV files)

>>> let c' = cache' "/tmp/cached-ex/c" c

    Running the cached computation "c'" will execute all necessary
    computations and write results to files as expected:

>>> -- Before running, let's make sure the target directory is clean.
>>> :!mkdir -p /tmp/cached-ex
>>> :!rm -f /tmp/cached-ex/*
>>> :!echo 2 > /tmp/cached-ex/b
>>>
>>> runShake c'
# Writing cache (for /tmp/cached-ex/a)
# Writing cache (for /tmp/cached-ex/c)
Build completed in 0:01m
...

    Running it again won't run anything. Since none of the cached or
    source files have changed, there is nothing to re-compute.

>>> runShake c'
Build completed in 0:01m
...

    If we modify the content of "\/tmp\/cached-ex\/b", running "c'"
    again will only re-run the computation for "c'", and not for "a"
    since it does not depend on "b".

>>> :! echo 3 > /tmp/cached-ex/b
>>> runShake c'
# Writing cache (for /tmp/cached-ex/c)
Build completed in 0:01m
...

    The cache files and dependencies can be inspected with
    'prettyCached'. 

>>> prettyCached c' >>= putStr
Cached Value = 4
Cached Needs:
  /tmp/cached-ex/c
Cached Build:
  /tmp/cached-ex/a
  /tmp/cached-ex/c
    /tmp/cached-ex/a
    /tmp/cached-ex/b

    The previous output means that the value carried by "c'" is 4, and that in order to be computed, it needs the file "\/tmp\/cached-ex\/c". The last field, "Cached Build", lists each file that is to be built by the cache system and their dependencies: "\/tmp\/cache-ex\/a" and "\/tmp\/cache-ex\/c"
    are created by the cache system, and the latter depends on
    "\/tmp\/cache-ex\/a" and "\/tmp\/cache-ex\/b"

    To put together different independent cached values into a single one so that they all get built together, use '<>'. However, "Cached a" is an instance of Semigroup only if "a" is. You can "sink" the cached values first, which will turn a "Cache a" into a "Cache ()", satisfying the Semigroup constraint.
 
>>> let d = sink' "/tmp/cached-ex/d" (pure 'd')
>>> let e = sink' "/tmp/cached-ex/e" (pure 'e')
>>> let de = d <> e
>>> prettyCached de >>= putStr
Cached Value = ()
Cached Needs:
Cached Build:
  /tmp/cached-ex/d
  /tmp/cached-ex/e

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Cached (
  -- * Cached type
    Cached
  , getValue
  -- * Creation
  -- $creation
  , source
  , source'
  , fromIO
  -- * Caching values
  -- $caching
  , cache
  , cache'
  , cacheIO
  , sink
  , sink'
  , sinkIO
  , trigger
  -- * Running
  , toShakeRules
  , runShake
  -- * Showing
  , prettyCached
) where
    
import Protolude

import Control.Monad.Fail
import qualified Data.Set as Set
import Data.Text
import qualified Development.Shake as Shake

import Data.Cached.Internal

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude (read)


-- * Cached

-- | Extract the value cached value.
getValue :: Cached a -> IO (Either Text a)
getValue (CacheFail err) = return (Left err)
getValue (Cached a _ _) = runExceptT a


-- $creation To create cached values from pure values, use 'pure'.
--
-- >>> let a = pure 1 :: Cached Int
-- >>> prettyCached a >>= putStr
-- Cached Value = 1
-- Cached Needs:
-- Cached Build:
--
-- Newly created cached values won't actually be written to files or
-- attached to IO actions yet, as denoted by the empty "Cached Build"
-- field.  To associate them with actual cache files on disk, see
-- section "Caching values" below.
--
-- The following functions offer additionnal creation possibilities.
--

-- | Create a cached value from an input file.
source :: FilePath -> (Text -> Either Text a) -> Cached a
source path fromText = Cached
  { cacheRead = ExceptT $ first errMsg . fromText <$> readFile path
  , cacheNeeds = Set.singleton path
  , cacheBuild = mempty }
  where errMsg e = "Error reading file " <> pack path <> ": " <> e

-- | A convenient variant of 'source' when the type of the value to be read
-- instantiates 'Read'.
source' :: (Read a) => FilePath -> Cached a
source' path = source path fromText
  where fromText = bimap pack identity . readEither . unpack

-- | Create a cached value from an IO action that depends on some input files.
fromIO :: Set FilePath -> IO a -> Cached a
fromIO needs io = Cached (lift io) needs mempty


-- $caching
-- These functions associate cached values to files on disk or IO actions.

-- | Associate a cached value to a file on disk.
cache :: FilePath -> (a -> Text) -> (Text -> Either Text a) -> Cached a
      -> Cached a
cache path toText fromText = cacheIO path
                                     ( writeFile path . toText )
                                     ( fromText <$> readFile path )

-- | A convenient variant of 'cache' when the type of the value to be read is
-- an instance of 'Read' and 'Show'.
cache' :: (Show a, Read a) => FilePath -> Cached a -> Cached a
cache' path = cache path show (bimap pack identity . readEither . unpack)

-- | Caching with arbitrary IO actions.
cacheIO :: FilePath -> (a -> IO ()) -> (IO (Either Text a)) -> Cached a
        -> Cached a
cacheIO _ _ _ (CacheFail err) = CacheFail err
cacheIO path write read a = if isBuilt path (cacheBuild a)
  then CacheFail ("The cache file already exists: " <> pack path)
  else Cached { cacheRead = ExceptT $ (fmap . first) errMsg $ read
              , cacheNeeds = Set.singleton path
              , cacheBuild = buildSingle path
                               ( cacheRead a >>= lift . write )
                               ( cacheNeeds a )
                          <> cacheBuild a }
  where errMsg e = "Error reading file " <> pack path <> ": " <> e

-- | Associate a cached value to a file on disk without the possibility
-- to read it back. Useful for storing to a text file the final result of a
-- computation that doesn't need to be read again, like data for producing
-- figures, text output, etc.
sink :: FilePath -> (a -> Either Text Text) -> Cached a -> Cached ()
sink path toText = sinkIO path write
  where write = traverse (writeFile path) . toText

-- | A convenient variant of 'sink' when the written value type instantiates
-- 'Show'.
sink' :: (Show a) => FilePath -> Cached a -> Cached ()
sink' path = sink path (return . show)

-- | Sink with an arbitrary IO action.
sinkIO :: FilePath -> (a -> IO (Either Text ())) -> Cached a -> Cached ()
sinkIO _ _ (CacheFail err) = CacheFail err
sinkIO path write a = if isBuilt path (cacheBuild a)
  then CacheFail ("The cache file already exists: " <> pack path)
  else Cached { cacheRead = return ()
              , cacheNeeds = mempty
              , cacheBuild = buildSingle path
                                         (cacheRead a >>= ExceptT . write)
                                         (cacheNeeds a)
                          <> cacheBuild a}

-- | Trigger an IO action that depends on a set of files. For example, 
-- consider an executable "plot" that processes the content of a file
-- "data.csv" and writes an image to "fig.png". The figure creation can
-- be integrated into the cache system like so:
--
-- >>> import System.Process (callCommand)
-- >>> let t = trigger "fig.png" (return <$> callCommand "plot") (Set.fromList ["data.csv"])
-- >>> prettyCached t >>= putStr
-- Cached Value = ()
-- Cached Needs:
-- Cached Build:
--   fig.png
--     data.csv
-- 
trigger :: FilePath -> IO (Either Text ()) -> Set FilePath -> Cached ()
trigger path action needs = sinkIO path (\_ -> action) (fromIO needs (return ()))


-- ** Building

-- | Get shake 'Rules'. Those can be mixed together with other shake rules.
toShakeRules :: Cached a -> Shake.Rules ()
toShakeRules (CacheFail err) = Shake.action $ fail $ unpack err
toShakeRules a = buildShakeRules $ cacheBuild a

-- | Run the cached computation using shake (see <shakebuild.com>, "Development.Shake"). If you use the result of this function as your program's main, you can pass shake options as arguments. Try "my-program --help"
runShake :: Cached a -> IO ()
runShake a = Shake.shakeArgs Shake.shakeOptions
               { Shake.shakeThreads=0
               , Shake.shakeChange=Shake.ChangeModtimeAndDigest }
               ( toShakeRules a )

-- ** Pretty printing

prettyCached :: (Show a) => Cached a -> IO Text
prettyCached (CacheFail err) = return $ "CacheFail " <> err
prettyCached (Cached r n b) = do
  er <- runExceptT r
  case er of
    Left msg -> fail $ unpack msg
    Right r' ->  return $ "Cached Value = " <> show r' <> "\n"
                       <> "Cached Needs: \n"
                       <> foldMap (\p -> "  " <> pack p <> "\n") n
                       <> "Cached Build: \n"
                       <> (unlines $ fmap ("  " <>) (lines $ prettyBuild b))

