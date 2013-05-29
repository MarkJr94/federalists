import Data.List
import Preprocess
import Estimator
import Control.Monad
import Control.Applicative
import System.IO

removeItem :: Eq a => a -> [a] -> [a]
removeItem x = filter (x /=)

getModel :: FilePath -> IO Model
getModel path = liftM generateModel $ readFile path
    where generateModel = (flip buildEstimator) gamma . getCounts . words

getModelMulti :: [FilePath] -> IO Model
getModelMulti paths = estimate . counts <$> sequence (map readFile paths)
    where
        counts = getCounts . words . concat
        estimate = flip buildEstimator gamma

fullMadison :: IO Model
fullMadison = getModelMulti madNames

fullHamilton :: IO Model
fullHamilton = getModelMulti hamNames

label :: [FilePath] -> IO [(FilePath, [Char], Double, Double)]
label paths = 
    forM paths (\p -> do
        counts <- liftM (getCounts . words) $ readFile p
        ham_l <- liftM (flip logLikelihood counts) fullHamilton
        mad_l <- liftM (flip logLikelihood counts) fullMadison
        let label' = if ham_l > mad_l then "Hamilton" else "Madison"
        return (p, label', ham_l, mad_l) ) 

crossValOne :: [String] -> IO Model -> IO [(FilePath, [Char], Double, Double)]
crossValOne allNames fullModel = 
    forM allNames (\p -> do
            testModel <- getModelMulti $ removeItem p allNames
            let testCounts = getCounts $ p:[] 
            ham_l <- liftM (flip logLikelihood testCounts) fullModel
            let mad_l = logLikelihood testModel testCounts
            let label' = if ham_l > mad_l then "Hamilton" else "Madison"
            return (p, label', ham_l, mad_l))

crossValidate = liftM concat $ sequence 
    [crossValOne hamNames fullHamilton, crossValOne madNames fullMadison]

dump :: FilePath -> [(FilePath, String, Double, Double)] -> IO ()
dump path results = do
    out <- openFile path WriteMode
    hPutStrLn out "Subject \tLabel assigned\t Log likelihood [Hamilton model]\t\
    \Log likelihood [Madison model]"
    mapM_ (\(n,l,h, m) -> hPutStrLn out $ n ++ "\t" ++ l ++ "\t" ++ show h ++ 
        "\t" ++ show m) results

main :: IO ()
main = do
    label uNames >>= dump "labels.txt"
    crossValidate >>= dump "cross-validation.txt"
