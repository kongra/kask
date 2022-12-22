module Lexicon where

import qualified Data.HashSet as Set
import           Data.List
import           Prelude

lexicon :: Set.HashSet String
lexicon = Set.fromList ["foo", "bar", "baz"]

inLexicon :: String -> Bool
inLexicon s = Set.member s lexicon

prefsAndSuffs :: String -> Bool
prefsAndSuffs text =
  let lst = zip (inits text) (tails text) in
    any (\(l, r) -> inLexicon l && (inLexicon r || prefsAndSuffs r)) lst
