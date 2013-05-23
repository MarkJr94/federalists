

import Data.List
import Preprocess
import Estimator
import Control.Monad
import System.IO

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

getModel :: FilePath -> IO Model
getModel path = readFile path >>=
		return . getCounts . words >>=
		return . (flip buildEstimator) gamma

getModelMulti :: [FilePath] -> IO Model
getModelMulti paths = sequence (map readFile paths) >>=
		      return . getCounts . words . concat >>=
		      return . flip (buildEstimator) gamma

fullMadison :: IO Model
fullMadison = getModelMulti madNames

fullHamilton :: IO Model
fullHamilton = getModelMulti hamNames


label :: [FilePath] -> IO [(FilePath, [Char], Double, Double)]
label paths = forM paths (\p -> do
		counts <- readFile p >>= return . getCounts . words
		ham_l <- return . (flip logLikelihood counts) =<< h
		mad_l <- return . (flip logLikelihood counts) =<< m
		label' <- return $ if ham_l > mad_l then "Hamilton" else "Madison"
		return (p, label', ham_l, mad_l) ) 
	where m = fullMadison
	      h = fullHamilton

crossValMadison :: IO [(FilePath, [Char], Bool, Double, Double)]
crossValMadison = forM madNames (\p -> do
			testModel <- getModelMulti $ removeItem p madNames
			testCounts <- return . getCounts $ p:[] 
			ham_l <- return . (flip logLikelihood testCounts) =<< fullHamilton
			mad_l <- return $ logLikelihood testModel testCounts
			return ( if ham_l > mad_l then ("Hamilton",False)
						  else ("Madison",True) ) >>=
				(\(l, truth) -> return (p, l, truth, ham_l, mad_l)) )

crossValHamilton :: IO [(FilePath, [Char], Bool, Double, Double)]
crossValHamilton = forM hamNames (\p -> do
			testModel <- getModelMulti $ removeItem p hamNames
			testCounts <- return . getCounts $ p:[] 
			mad_l <- return . (flip logLikelihood testCounts) =<< fullMadison
			ham_l <- return $ logLikelihood testModel testCounts
			return ( if ham_l > mad_l then ("Hamilton", True)
						  else ("Madison", False) ) >>=
				(\(l, truth) -> return (p, l, truth, ham_l, mad_l)) )

crossValidate = sequence [crossValHamilton, crossValMadison] >>= return . concat

dumpLabel :: [(FilePath, String, Double, Double)] -> IO ()
dumpLabel labels = do
	out <- openFile "labels.txt" WriteMode
	hPutStrLn out "Subject \tLabel assigned\t Log likelihood [Hamilton model]\t\
	\Log likelihood [Madison model]"
	mapM_ (\(n,l,h, m) -> hPutStrLn out $ n ++ "\t" ++ l ++ "\t" ++ show h ++ 
		"\t" ++ show m) labels

dumpCrossVal :: [(FilePath, String, Bool, Double, Double)] -> IO ()
dumpCrossVal results = do
	out <- openFile "cross-validation.txt" WriteMode
	hPutStrLn out "Subject \tLabel assigned\t Log likelihood [Hamilton model]\t\
	\Log likelihood [Madison model]"
	mapM_ (\(n,l,_,h, m) -> hPutStrLn out $ n ++ "\t" ++ l ++ "\t" ++ show h ++ 
		"\t" ++ show m) results

main :: IO ()
main = label uNames >>= dumpLabel >>
	crossValidate >>= dumpCrossVal
