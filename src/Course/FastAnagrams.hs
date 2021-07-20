{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import Course.Monad
import Course.Applicative
import qualified Data.Set as S
import Data.Ord

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams name fileName = ((ncString <$>) <$>)
                             ((flip (filter . flip S.member)
                              (NoCaseString <$> (permutations name)) .
                              S.fromList . hlist .
                              (\x -> NoCaseString <$> (lines x))) <$>
                             readFile fileName)

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString

instance Ord NoCaseString where
  compare = on compare ((toLower <$> ) . ncString)
