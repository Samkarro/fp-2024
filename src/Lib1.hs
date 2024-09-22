module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = ["single", "artist", "track-collection", "ep", "ep-name", "ab", "ab-name", "lp", "lp-name", "alias", "track-name"]