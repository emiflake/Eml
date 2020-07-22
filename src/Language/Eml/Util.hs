module Language.Eml.Util where

sepBy :: String -> [String] -> String
sepBy sep els =
  case els of
    [] -> ""
    [x] -> x
    (x : xs) -> x ++ sep ++ sepBy sep xs
