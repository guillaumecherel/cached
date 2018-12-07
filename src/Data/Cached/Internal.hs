{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Cached.Internal where

import Protolude

import Control.Monad.Fail
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text
import Development.Shake

-- |A value that is produced from files on disk or arbitrary IO actions.
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

-- * Build

newtype Build = Build {getBuild :: Map FilePath (ExceptT Text IO (), Set FilePath)}

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

buildShakeRules :: Build -> Rules ()
buildShakeRules b = do
            want (buildTargets b)
            foldMap buildOne ( buildList b )
  where buildOne (outPath, write, needs) =
                    outPath %> \_ -> do
                      Development.Shake.need (Set.toList needs)
                      e <- traced "Writing cache" $ runExceptT write
                      case e of
                        Right () -> return ()
                        Left err -> fail $ "Error running shake rule building file " <> outPath <> "depending on " <> show needs <> "\nError message: "<> unpack err

prettyBuild :: Build -> Text
prettyBuild (Build a) = foldMap showOne $ Map.toList a
  where showOne :: (FilePath, (ExceptT Text IO (), Set FilePath)) -> Text
        showOne (target, (_, needs)) =
             pack target <> "\n"
          <> (unlines $ fmap (\n -> "  " <> pack n) $ Set.toList needs)


