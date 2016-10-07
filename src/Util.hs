
-- These are basic utility functions which don't belong elsewhere
module Util where

-- | Removes the first instance of a list for which the supplied predicate
-- returns true
removeElem :: (a -> Bool) -> [a] -> [a]
removeElem p xs = a ++ as
                where (a, _:as) = span p xs

