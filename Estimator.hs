module Estimator
(	Prob,
	Model,
	gamma,
	buildEstimator,
	logLikelihood
) where

import qualified Data.Map as M
import Data.List
import Preprocess

data Prob = Prob { count :: Int
	    , theta :: Double
	    } deriving (Show)

type Model = M.Map String Prob

gamma = 0.25 :: Double

buildEstimator :: Dict -> Double -> Model
buildEstimator dict gamma = M.map (\v -> Prob { count = v, theta = (f v + gamma)/denom}) dict
	where counts = M.elems dict
	      f = fromIntegral
	      k = length counts
	      denom = f k * gamma + f ( sum counts)

logLikelihood :: Model -> Dict -> Double
logLikelihood m d = M.foldrWithKey helper 0.0 d
	where f = fromIntegral
	      z = 1.0 / ( f $ M.size m)
	      helper k v res = res + f v * if M.member k m then (theta $ m M.! k) else z
