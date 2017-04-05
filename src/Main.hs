module Main where

import Parser
import qualified Data.MultiSet as M
import Control.Lens
import qualified Data.Text.Lazy.IO as I
import Data.List (sortBy)
import Data.Function (on)
import Text.Megaparsec (parseMaybe)

file :: String
file = "/home/bertram/Downloads/Bonnsai.txt"

messages :: IO [Chunk]
messages = filter isMessage <$> ((parseMaybe chunksP <$> I.readFile file)
           >>= maybe (putStrLn "Parse failed" >> return []) return)

main :: IO ()
main = do
  msg <- messages
  let users = map (toText . (^.user)) msg
  let counts = M.fromList users
  mapM_ (\(a,b)->I.putStr a >> putStr " | " >> print b) . sortBy (compare `on` snd) . M.toOccurList $ counts
