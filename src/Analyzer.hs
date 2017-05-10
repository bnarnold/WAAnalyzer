module Analyzer where

import Parser
import Control.Lens
import qualified Data.Map as M
import Numeric.LinearAlgebra

dictToMatrix :: M.Map User Int 
             -> M.Map (User,User) Int 
             -> (Matrix R,[User])
dictToMatrix ud uud = ((n><n) probs,map fst ucount)
  where
    n = length ud
    uucount = M.toList uud
    ucount = M.toList ud
    uutotal = [(u,u',n) | (u,n) <- ucount, (u',_) <- ucount]
    probs = combine uutotal uucount
    combine :: [(User,User,Int)] -> [((User,User),Int)] -> [R]
    combine _ [] = []
    combine [] as = map (const 0) as
    combine ((u,u',n):ts) olds@(((v,v'),k):news)
      | u == v && u' == v' = fromIntegral k/fromIntegral n : combine ts news
      | otherwise = 0 : combine ts olds

matrixP = uncurry dictToMatrix . (\(d,d') -> (fst <$> d,d')) <$> dictP
