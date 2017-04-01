{-# LANGUAGE TemplateHaskell
            ,TupleSections
            ,OverloadedStrings
            #-}

module Parser where

import Data.Dates
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as I
import Text.Megaparsec
import Text.Megaparsec.Text.Lazy
import Control.Lens
import Control.Applicative (liftA2,liftA3)


data User = User T.Text | Number T.Text deriving (Show,Eq)
data Message = Join User -- added by User
             | Quit -- Left
             | Media -- Sent Media
             | IconChange -- Changed Icon
             | NumberChange -- Changed Number
             | Create -- Created group
             | Change -- Changed group subject
             | Message T.Text -- Sent Message
             deriving (Show)
data Chunk = Chunk
             { _time :: DateTime
             , _user :: User
             , _message :: Message
             } deriving (Show)
makeLenses ''Chunk

userP :: Parser a -> Parser User
userP end = Number . T.pack <$> liftA2 (:) (char '\8234' *> char '+')
            (manyTill (digitChar <|> spaceChar) (char '\8236' *> end))
            <|> User . T.pack <$> manyTill noNewlineChar end
  where
    noNewlineChar = notFollowedBy eol *> anyChar
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

firstLineP :: Parser Chunk
firstLineP = liftA2 (\d (u,m) -> Chunk d u m) (dateP <* string " - ")
                    userMessageP
  where
    userMessageP :: Parser (User,Message)
    userMessageP =   try createP
                 <|> try joinP
                 <|> try quitP
                 <|> try changeP
                 <|> try numberChangeP
                 <|> try iconChangeP
                 <|> try mediaP
                 <|> firstLineP'
    createP = (,Create) <$> userP (string " created group ")
                    <* manyTill anyChar eolf
    joinP = liftA2 join' (userP (string " added ")) (userP eolf)
    join' u1 u2 = (u2, Join u1)
    quitP = (,Quit) <$> userP (string " left" <* eolf)
    changeP = (,Change) <$> userP (string " changed the subject to ")
                    <* manyTill anyChar eolf
    iconChangeP = (,IconChange) <$> userP
      (string " changed this group's icon" <* eolf)
    numberChangeP = (,NumberChange) <$>
      ((try (userP (string " changed from"))
      <|> userP (string " changed to "))
      <* manyTill anyChar eolf)
    mediaP = (,Media) <$> userP (string ": <Media omitted>" <* eolf)
    firstLineP' = liftA2 mess' (userP (string ": ")) (manyTill anyChar eolf)
    mess' u t = (u, Message $ T.pack t)
    eolf = eof <|> (eol *> return ())

chunksP :: Parser [Chunk]
chunksP =   eof *> return [] <|> (firstLineP >>= chunksP')
  where
    chunksP' :: Chunk -> Parser [Chunk]
    chunksP' c@(Chunk _ _ m) = eof *> return [c] <|>
      case m of  
        Message t -> remNextChunk >>= updateAndParse
          where
            updateAndParse (ls,mc) = case mc of
              Nothing  -> return [c']
              Just c'' -> (c':) <$> chunksP' c''
              where
                c' = c & message .~ (Message . T.intercalate "\n" $ t:ls)
        _ -> (c:) <$> chunksP
remNextChunk =   eof *> return ([],Nothing)
             <|> ([],) . Just <$> try firstLineP
             <|> liftA2 (\l (ls,c) -> (l:ls,c)) line remNextChunk
line = T.pack <$> manyTill anyChar (eof <|> eol *> return ())

isMedia :: Message -> Bool
isMedia Media = True
isMedia _     = False

isUser :: User -> Bool
isUser (User _)   = True
isUser (Number _) = False
      

test :: (Show a) => Parser a -> String -> IO ()
test p = parseTest p . T.pack

file :: String
file = "/home/bertram/Downloads/Bonnsai.txt"

bertram :: User
bertram = User $ T.pack "Bertram"

messages :: IO [Chunk]
messages = (parseMaybe chunksP <$> I.readFile file)
           >>= maybe (putStrLn "Parse failed" >> return []) return
