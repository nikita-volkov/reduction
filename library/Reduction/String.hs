module Reduction.String
where

import Reduction.Prelude


attoFailure :: [String] -> String -> String
attoFailure context details = case context of
  [] -> details
  _ -> intercalate " > " context <> ": " <> details
