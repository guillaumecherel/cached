{-| This module lets you cache values to disk to avoid re-running (potentially
    long) computations between consecutive executions of your
    program. Cached values are recomputed only when needed, i.e. when
    other cached values on which they depend change. Independant
    computations can be run in parallel for free.

    The module was motivated by writing scientific data flows, simulation
    experiments or data science scripts. Those often involve long
    computations and create "flows" where the output of some computation
    are the inputs of others, until final results are produced (values,
    figures, statistical tests, etc.).

    A value of type "Cached a" should be understood as a value of type "a" that is read from a file, or that is produced from data stored in one or more files.

    Cached values can be created from pure values,

>>> let a = cache' "/tmp/cached-ex/a" (pure (1 :: Int))
>>> :t a
a :: Cached Int

    or from files stored on disk.

>>> import Data.Text.Read (decimal)
>>> import Data.Text (pack)
>>> let readInt = bimap pack fst . decimal :: Text -> Either Text Int
>>>
>>> let b = source "/tmp/cached-ex/b" readInt
>>> :t b
b :: Cached Int

    Use the functor and applicative instances to use cached values in
    new computations.

>>> let bigA = fmap (* 2) a
>>> let c = (+) <$> a <*> b

    The cached value "c" represents a value produced by summing
    together the values stored in "\/tmp\/cached-ex\/a" and
    "\/tmp\/cached-ex\/b". It is not yet associated to its own cache
    file. To actually store it into a cache file, use the function
    'cache'' (or 'cache' for values that are not instances of Show and
    Read or when you want to control the file format, e.g. writing arrays
    to CSV files)

>>> let c' = cache' "/tmp/cached-ex/c" c

    Running the cached computation "c'" will execute all necessary
    computations and write results to files as expected:

>>> -- Before running, let's make sure the target directory is clean.
>>> :!mkdir -p "/tmp/cache-ex"
>>> :!rm -f /tmp/cached-ex/*
>>> :!echo "2" > "/tmp/cached-ex/b"
>>>
>>> runCached c'
# Writing cache (for /tmp/cached-ex/a)
# Writing cache (for /tmp/cached-ex/c)
Build completed in 0:01m
...

    Running it again won't run anything. Since none of the cached or
    source files have changed, there is nothing to re-compute.

>>> runCached c'
Build completed in 0:01m
...

    If we modify the content of "\/tmp\/cached-ex\/b", running "c'"
    again will only re-run the computation for "c'", and not for "a"
    since it does not depend on "b".

>>> :! echo 3 > /tmp/cached-ex/b
>>> runCached c'
# Writing cache (for /tmp/cached-ex/c)
Build completed in 0:01m
...

    The cache files and dependencies can be inspected with
    'prettyCached'. Here, "\/tmp\/cache-ex\/a" and "\/tmp\/cache-ex\/c"
    are created by the cache system, and the latter depends on
    "\/tmp\/cache-ex\/a" and "\/tmp\/cache-ex\/b"

>>> prettyCached c' >>= putStrLn
Cached Value = 4
Cached Needs:
  /tmp/cached-ex/c
Cached Build:
  /tmp/cached-ex/a
  /tmp/cached-ex/c
    /tmp/cached-ex/a
    /tmp/cached-ex/b
...

    To put together different independent cached values into a single one so that they all get built together, use '<>'. However, "Cached a" is an instance of Semigroup only if "a" is. You can "sink" the cached values first, which will turn a "Cache a" into a "Cache ()", satisfying the Semigroup constraint.
 
>>> let d = sink' "/tmp/cached-ex/d" (pure 'd')
>>> let e = sink' "/tmp/cached-ex/e" (pure 'e')
>>> let de = d <> e
>>> runCached de
# Writing cache (for /tmp/cached-ex/d)
# Writing cache (for /tmp/cached-ex/e)
Build completed in 0:01m
...
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Cached (
  -- * Cached type
    Cached
  -- * Creation
  --
  -- | To create cached values from pure values, use 'pure'. Then, to associate
  -- them with actual cache files on disk, use 'cache' or 'cache''. The
  -- following functions offer additionnal creation possibilities.
  , fromIO
  , source
  , source'
  -- * Associate cached values to cache files on disk
  , cache
  , cache'
  , sink
  , sink'
  , sinkEither
  , tag
  -- * Running
  , buildCache
  , runCached
  -- * Showing
  , prettyCached
) where
    
import Protolude

import Control.Monad.Fail
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text
import Development.Shake



-- * Cached

-- |A value that is produced from files on disk.
data Cached a = Cached { cacheRead :: ExceptT Text IO a
                       , cacheNeeds :: Set FilePath
                       , cacheBuild :: Build }
              | CacheFail Text

instance Functor Cached where
  fmap _ (CacheFail err) = CacheFail err
  fmap f (Cached r n b) = Cached (fmap f r) n b

instance Applicative Cached where
  pure a = Cached (return a) mempty mempty

  (CacheFail err) <*> _ = CacheFail err
  _ <*> (CacheFail err) = CacheFail err
  f <*> a = Cached ( cacheRead f <*> cacheRead a )
                  ( cacheNeeds f <> cacheNeeds a )
                  (  cacheBuild f <> cacheBuild a )

instance (Semigroup a) => Semigroup (Cached a) where
  (CacheFail err) <> _ = CacheFail err
  _ <> (CacheFail err) = CacheFail err
  (Cached cr1 cn1 cb1) <> (Cached cr2 cn2 cb2) =
    Cached { cacheRead = liftA2 (<>) cr1 cr2
          , cacheNeeds = cn1 <> cn2
          , cacheBuild = cb1 <> cb2 }

instance (Monoid a) => Monoid (Cached a) where
  mempty = Cached (return mempty) mempty mempty

instance Num a => Num (Cached a) where
    fromInteger = pure . fromInteger
    {-# INLINE fromInteger #-}

    negate = fmap negate
    {-# INLINE negate #-}

    abs = fmap abs
    {-# INLINE abs #-}

    signum = fmap signum
    {-# INLINE signum #-}

    (+) = liftA2 (+)
    {-# INLINE (+) #-}

    (*) = liftA2 (*)
    {-# INLINE (*) #-}

    (-) = liftA2 (-)
    {-# INLINE (-) #-}

instance Fractional a => Fractional (Cached a) where
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

    recip = fmap recip
    {-# INLINE recip #-}

    (/) = liftA2 (/)
    {-# INLINE (/) #-}

instance Floating a => Floating (Cached a) where
    pi = pure pi
    {-# INLINE pi #-}

    exp = fmap exp
    {-# INLINE exp #-}

    sqrt = fmap sqrt
    {-# INLINE sqrt #-}

    log = fmap log
    {-# INLINE log #-}

    sin = fmap sin
    {-# INLINE sin #-}

    tan = fmap tan
    {-# INLINE tan #-}

    cos = fmap cos
    {-# INLINE cos #-}

    asin = fmap asin
    {-# INLINE asin #-}

    atan = fmap atan
    {-# INLINE atan #-}

    acos = fmap acos
    {-# INLINE acos #-}

    sinh = fmap sinh
    {-# INLINE sinh #-}

    tanh = fmap tanh
    {-# INLINE tanh #-}

    cosh = fmap cosh
    {-# INLINE cosh #-}

    asinh = fmap asinh
    {-# INLINE asinh #-}

    atanh = fmap atanh
    {-# INLINE atanh #-}

    acosh = fmap acosh
    {-# INLINE acosh #-}

    (**) = liftA2 (**)
    {-# INLINE (**) #-}

    logBase = liftA2 logBase
    {-# INLINE logBase #-}

-- | Create a cached value from an IO action that depends on some input files.
fromIO :: Set FilePath -> IO a -> Cached a
fromIO needs io = Cached (lift io) needs mempty

-- | Create a cached value from an input file.
source :: FilePath -> (Text -> Either Text a) -> Cached a
source path fromText = Cached
  { cacheRead = ExceptT $ fromText <$> readFile path
  , cacheNeeds = Set.singleton path
  , cacheBuild = mempty }

-- | A convenient variant of 'source' when the type of the value to be read
-- instantiates 'Read'.
source' :: (Read a) => FilePath -> Cached a
source' path = source path fromText
  where fromText = bimap pack identity . readEither . unpack

-- need :: FilePath -> Cached ()
-- need path = Cached
--   { cacheRead = return ()
--   , cacheNeeds = Set.singleton path
--   , cacheBuild = mempty }

-- | Associate a cached value to a file on disk.
cache :: FilePath -> (a -> Text) -> (Text -> Either Text a) -> Cached a
         -> Cached a
cache _ _ _ (CacheFail err) = CacheFail err
cache path toText fromText a = if isBuilt path (cacheBuild a)
  then CacheFail ("The cache file already exists: " <> pack path)
  else Cached { cacheRead = ExceptT $ fromText <$> readFile path
             , cacheNeeds = Set.singleton path
             , cacheBuild = buildSingle path
                              ( toText <$> cacheRead a
                                >>= lift . writeFile path)
                              ( cacheNeeds a )
                         <> cacheBuild a }

-- | A convenient variant of 'cache' when the type of the value to be read is
-- an instance of 'Read' and 'Show'.
cache' :: (Show a, Read a) => FilePath -> Cached a -> Cached a
cache' path = cache path show (bimap pack identity . readEither . unpack)

-- | Associate a cached value to a file on disk without the possibility
-- to read it back. Useful for storing to a text file the final result of a
-- computation that doesn't need to be read again, like data for producing
-- figures, text output, etc.
sink :: FilePath -> (a -> Text) -> Cached a -> Cached ()
sink path toText = sinkIO path write
  where write a = lift $ writeFile path $ toText a

-- | For when caching may fail depending on the value to cache.
sinkEither :: FilePath -> (a -> Either Text Text) -> Cached a -> Cached ()
sinkEither path toText = sinkIO path write
  where write a = ExceptT $ traverse (writeFile path) (toText a)

-- | A convenient variant of 'sink' when the written value type instantiates
-- 'Show'.
sink' :: (Show a) => FilePath -> Cached a -> Cached ()
sink' path = sink path show

sinkIO :: FilePath -> (a -> ExceptT Text IO ()) -> Cached a -> Cached ()
sinkIO _ _ (CacheFail err) = CacheFail err
sinkIO path write a = if isBuilt path (cacheBuild a)
  then CacheFail ("The cache file already exists: " <> pack path)
  else Cached { cacheRead = return ()
              , cacheNeeds = mempty
              , cacheBuild = buildSingle path
                                         (cacheRead a >>= write)
                                         (cacheNeeds a)
                          <> cacheBuild a}

-- | Artificially associate a cached value to a "tag" file. It is
-- sometimes useful to use IO actions to create files, such as files that
-- are created by external commands. One can use this function to do so. For
-- example, consider an external commant "plot" that processes the content
-- of a file "data.csv" and writes an image to "fig.png". The figure creation 
-- can be integrated into the cache system like so:
--
-- >>> import System.Process (callCommand)
-- >>> let t = tag "tag_fig" $ fromIO (Set.fromList ["data.csv"]) (callCommand "plot")
-- >>> prettyCached t >>= putStrLn
-- Cached Value = ()
-- Cached Needs:
-- Cached Build:
--   tag_fig
--     data.csv
-- ...
tag :: FilePath -> Cached () -> Cached ()
tag path = sinkIO path (\_ -> return ())




-- ** Building

-- | Get shake 'Rules'.
buildCache :: Cached a -> Rules ()
buildCache (CacheFail err) = action $ fail $ unpack err
buildCache a = build $ cacheBuild a

-- | Run the cached computation using shake.
runCached :: Cached a -> IO ()
runCached a = shakeArgs shakeOptions{shakeThreads=0} (buildCache a)

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
                       <> foldMap (\l -> "  " <> l <> "\n")
                                  (lines $ prettyBuild b)


-- * Build

newtype Build = Build (Map FilePath (ExceptT Text IO (), Set FilePath))

instance Semigroup Build where
  (Build a) <> (Build b) = Build (a <> b)

instance Monoid Build where
  mempty = Build Map.empty

isBuilt :: FilePath -> Build -> Bool
isBuilt p (Build m) = Map.member p m

buildSingle :: FilePath -> ExceptT Text IO () -> Set FilePath -> Build
buildSingle path write needs = Build $ Map.singleton path (write, needs)

buildList :: Build -> [(FilePath, ExceptT Text IO (), Set FilePath)]
buildList (Build m) = fmap (\(a, (b, c)) -> (a,b,c)) (Map.toList m)

buildTargets :: Build -> [FilePath]
buildTargets (Build m) = Map.keys m

build :: Build -> Rules ()
build b = do
            want (buildTargets b)
            foldMap buildOne ( buildList b )
  where buildOne (outPath, write, needs) =
                    outPath %> \_ -> do
                      Development.Shake.need (Set.toList needs)
                      e <- traced "Writing cache" $ runExceptT write
                      case e of
                        Right () -> return ()
                        Left err -> fail $ unpack err

prettyBuild :: Build -> Text
prettyBuild (Build a) = foldMap showOne $ Map.toList a
  where showOne :: (FilePath, (ExceptT Text IO (), Set FilePath)) -> Text
        showOne (target, (_, needs)) =
             pack target <> "\n"
          <> foldMap (\n -> "  " <> pack n <> "\n") needs



