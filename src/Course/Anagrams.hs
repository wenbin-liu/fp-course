{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor
import Course.Monad
import Course.Applicative

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
anagrams word fileName = readFile fileName >>=
                         \s -> pure (filter (equalIgnoringCase word) (lines s))


-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase a b = let a_lower = toLower <$> a
                            b_lower = toLower <$> b
                        in (intersectBy (==) (permutations a_lower)
                           (permutations b_lower)) /= Nil
