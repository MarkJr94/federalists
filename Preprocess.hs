module Preprocess
(	hamNames,
	madNames,
	uNames,
	getCounts,
	Dict
) where

import qualified Data.Map as M
import Data.Char
import Data.List

type Dict = M.Map String Int

nums = [1..15]
hamNames = ["federalist/hamilton" ++ (show n) ++ ".txt" | n <- nums]
madNames = ["federalist/madison" ++ (show n) ++ ".txt" | n <- nums]
uNames = ["federalist/unknown" ++ (show n) ++ ".txt" | n <- [1..11]]

removePunct :: String -> String
removePunct = filter $ not . isPunctuation

lower :: String -> String
lower = map toLower

clean :: String -> String
clean = removePunct . lower

getCounts :: [String] -> Dict
getCounts ss = go m ss
	where m = M.empty
	      go m' (s':ss') = go ( M.insertWith (+)  (clean s') 1 m') ss'
	      go m' _ = m'
