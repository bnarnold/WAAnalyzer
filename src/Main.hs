module Main where

import Parser
import Analyzer
import qualified Data.MultiSet as M
import Control.Lens hiding ((??),(|>),(<.>))
import qualified Data.Text.Lazy.IO as I
import Data.List (sortBy)
import Data.Function (on)
import Text.Megaparsec (parseMaybe)
import Numeric.LinearAlgebra
import Data.Complex (realPart)

file :: String
file = "/home/bertram/Downloads/Bonnsai.txt"

messages :: IO [Chunk]
messages = filter isMessage <$> ((parseMaybe chunksP <$> I.readFile file)
           >>= maybe (putStrLn "Parse failed" >> return []) return)

main' :: IO ()
main' = do
  msg <- messages
  let users = map (toText . (^.user)) msg
  let counts = M.fromList users
  mapM_ (\(a,b)->I.putStr a >> putStr " | " >> print b) . sortBy (compare `on` snd) . M.toOccurList $ counts

main :: IO ()
main = do
  input <- I.readFile file
  let (matrix,users) = maybe undefined id (parseMaybe matrixP input)
  let n = length users
  let probs =  cmap realPart . flatten . (?? (All, Take 1)) 
               . snd . eig . tr $ matrix
  let probs' = scale (1 / (probs <.> (n |> repeat 1))) probs
  mapM_ print . sortBy (compare `on` snd). zip users $ toList probs'
  print $ sum . toList $ probs'
