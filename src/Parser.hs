{-# LANGUAGE TemplateHaskell
            ,TupleSections
            ,OverloadedStrings
            #-}

module Parser where

import Data.Time
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as I
import Text.Megaparsec
import Text.Megaparsec.Text.Lazy
import Control.Lens
import Control.Applicative (liftA2,liftA3)
import qualified Data.Map as M

data User = User T.Text | Number T.Text deriving (Show,Eq,Ord)
data Message = Join User -- added by User
             | Join' -- Joined
             | Quit -- Left
             | Media -- Sent Media
             | IconChange -- Changed Icon
             | NumberChange -- Changed Number
             | Create -- Created group
             | Change -- Changed group subject
             | Encrypt -- WhatsApp message about encryption
             | Message T.Text -- Sent Message
             deriving (Show)
data Chunk = Chunk
             { _time :: UTCTime
             , _user :: User
             , _message :: Message
             } deriving (Show)
makeLenses ''Chunk

toText :: User -> T.Text
toText (User u)   = u
toText (Number n) = n

userP :: Parser a -> Parser User
userP end = Number . T.pack <$> liftA2 (:) (char '\8234' *> char '+')
            (manyTill (digitChar <|> spaceChar) (char '\8236' *> end))
            <|> User . T.pack <$> manyTill noNewlineChar end
  where
    noNewlineChar = notFollowedBy eol *> anyChar
dateP :: Parser UTCTime
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
    datify d m y h mm = UTCTime (day (toInteger y) m d) (offset h mm)
    day = fromGregorian
    offset h mm = secondsToDiffTime . toInteger $ 3600*h + 60+mm

firstLineP :: Parser Chunk
firstLineP = liftA2 (\d (u,m) -> Chunk d u m) (dateP <* string " - ")
                    userMessageP
  where
    userMessageP :: Parser (User,Message)
    userMessageP =   try firstLineP'
                 <|> try createP
                 <|> try joinP
                 <|> try joinP'
                 <|> try quitP
                 <|> try changeP
                 <|> try numberChangeP
                 <|> try iconChangeP
                 <|> encryptP
    createP = (,Create) <$> userP (string " created group ")
                    <* manyTill anyChar eolf
    joinP = liftA2 join' (userP (string " added ")) (userP eolf)
    join' u1 u2 = (u2, Join u1)
    joinP' = (,Join') <$> userP (string " was added" <* eolf)
    quitP = (,Quit) <$> userP (string " left" <* eolf)
    changeP = (,Change) <$> userP (string " changed the subject to ")
                    <* manyTill anyChar eolf
    iconChangeP = (,IconChange) <$> userP
      (string " changed this group's icon" <* eolf)
    encryptP = string ("Messages you send to this group are now secured "
                ++ "with end-to-end encryption. Tap for more info.")
               *> eolf *> return (User "WhatsApp",Encrypt)
    numberChangeP = (,NumberChange) <$>
      ((try (userP (string " changed from"))
      <|> userP (string " changed to "))
      <* manyTill anyChar eolf)
    firstLineP' = liftA2 (,) (userP (string ": "))
                  (try (string "<Media omitted>" *> eolf *> return Media)
                  <|> Message . T.pack <$> manyTill anyChar eolf)
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

isMessage :: Chunk -> Bool
isMessage (Chunk _ _ (Message _)) = True
isMessage (Chunk _ _ Media)       = True
isMessage _                       = False

dictP :: Parser (M.Map User (Int,NominalDiffTime),M.Map (User,User) Int)
dictP = toDict . filter isMessage <$> chunksP
  where
    toDict = toDict' (M.empty,M.empty)
    toDict' dd [] = dd
    toDict' dd [_] = dd
    toDict' (td,ud) (c:cs@(c':_))
      = let (Chunk d u _) = c
            (Chunk d' u' _) = c'
            dt = diffUTCTime d' d
        in toDict'
           (M.insertWith (\(n,t) (k,dt)->(n+k,t+dt)) u (1,dt) td
           ,M.insertWith (+) (u,u') 1 ud)
           cs
