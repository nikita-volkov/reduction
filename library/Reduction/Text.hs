module Reduction.Text
where

import Reduction.Prelude


attoFailure :: [String] -> String -> Text
attoFailure context details = fromString $ case context of
  [] -> details
  _ -> intercalate " > " context <> ": " <> details
