{-# LANGUAGE TemplateHaskell #-}

module Parser where

import Data.Dates
import qualified Data.Text.Lazy as T
import Text.Megaparsec
import Text.Megaparsec.Text.Lazy
import Control.Lens


data User = User T.Text | Number T.Text deriving (Show)
data Message = Media | Message T.Text deriving (Show)
data Chunk = Chunk
             { _time :: DateTime
             , _user :: User
             , _message :: Message
             } deriving (Show)
makeLenses ''Chunk

userP :: Parser User
userP = numberP <|> nameP
  where
    numberP = Number . T.pack <$> ((:) <$> char '+'
              <*> manyTill (digitChar <|> spaceChar) (char ':'))
    nameP = User . T.pack <$> manyTill anyChar (char ':')

dateP :: Parser DateTime
dateP = datify 
        <$> intP 2 -- day
        <* char '/'
        <*> intP 2 -- month
        <* char '/'
        <*> intP 4 -- year
        <* string ", "
        <*> intP 2 -- hour
        <* char ':'
        <*> intP 2
  where
    intP :: Int -> Parser Int
    intP n = read <$> count n digitChar
    datify d m y h mm = DateTime y m d h mm 0

messageP :: Parser Message
messageP = string "<Media omitted>" *> eol *> return Media
         <|> Message <$> (T.append <$> firstLineP <*> otherLineP)
  where
    firstLineP :: Parser T.Text
    firstLineP = T.pack <$> manyTill anyChar eol
    otherLineP :: Parser T.Text
    otherLineP = lookAhead (try (dateP <* string " - " <* userP *> return ())
                            <|> eof)
                 *> return (T.pack "")
                 -- if we find a valid new line, end message
                 <|> T.append <$> (T.pack . ('\n':) <$> manyTill anyChar eol)
                     <*> otherLineP
                 -- else parse line, append with newline, retry

chunkP = Chunk
         <$> dateP
         <* string " - "
         <*> userP
         <* space
         <*> messageP
      

test :: (Show a) => Parser a -> String -> IO ()
test p = parseTest p . T.pack

file :: String
file = "/home/bertram/Downloads/Bonnsai.txt"
