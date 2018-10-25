{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Cached where
  
import Protolude

import Control.Monad.Fail
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text
import Development.Shake



-- * Cached

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



-- ** Utility functions

liftIO :: IO a -> Cached a
liftIO io = Cached (lift io) mempty mempty

source :: FilePath -> (Text -> Either Text a) -> Cached a
source path fromText = Cached
  { cacheRead = ExceptT $ fromText <$> readFile path
  , cacheNeeds = Set.singleton path
  , cacheBuild = mempty }

need :: FilePath -> Cached ()
need path = Cached
  { cacheRead = return ()
  , cacheNeeds = Set.singleton path
  , cacheBuild = mempty }

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

cache' :: (Show a, Read a) => FilePath -> Cached a -> Cached a
cache' path = cache path show (bimap pack identity . readEither . unpack)

sink :: FilePath -> (a -> ExceptT Text IO ()) -> Cached a -> Cached ()
sink _ _ (CacheFail err) = CacheFail err
sink path write a = if isBuilt path (cacheBuild a)
  then CacheFail ("The cache file already exists: " <> pack path)
  else Cached { cacheRead = return ()
             , cacheNeeds = mempty
             , cacheBuild = buildSingle path
                                        (cacheRead a >>= write)
                                        (cacheNeeds a)
                         <> cacheBuild a}

sinkIO :: FilePath -> (a -> IO ()) -> Cached a -> Cached ()
sinkIO path write = sink path (lift . write)

sinkTxt :: FilePath -> (a -> Either Text Text) -> Cached a -> Cached ()
sinkTxt path toText = sink path (\a -> ExceptT (return $ toText a) >>= lift . writeFile path)



-- ** Building

buildCache :: Cached a -> Rules ()
buildCache (CacheFail err) = action $ fail $ unpack err
buildCache a = build $ cacheBuild a



-- ** Pretty printing

prettyCache :: (Show a) => Cached a -> Text
prettyCache (Cached _ n b) = "Cached Reads = ?" <> "\n"
                         <> "Cached Needs: \n"
                         <> foldMap (\p -> "  " <> show p <> "\n") n
                         <> "Cached Build: \n"
                         <> foldMap (\l -> "  " <> l <> "\n")
                                    (lines $ prettyBuild b)
prettyCache (CacheFail err) = "CacheFail " <> err




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



