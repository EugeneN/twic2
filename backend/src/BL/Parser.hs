{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module BL.Parser (parseTweet) where

import           BL.Types         (TweetElement (..))
import           Data.Text        (Text, pack, unpack)
import           Text.Parsec
import           Text.Parsec.Text (Parser)



urlAlphabet = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_!*'(),;:/?&=."

usernameAlphabet = ['a'..'z']++['A'..'Z']++['0'..'9']++"_"

hashtagAlphabet = ['a'..'z']++['A'..'Z']++['0'..'9']++"_"

whitespaces = " \t"

lb = do
  string "\n"
  return LineBreak

username = do
  char '@'
  u <- many1 $ oneOf usernameAlphabet
  return $ AtUsername $ pack u

link = do
  proto <- try (string "http://") <|> string "https://"
  url <- many1 (oneOf urlAlphabet)
  return $ Link $ pack $ proto ++ url


hashtag = do
  char '#'
  t <- many1 $ oneOf hashtagAlphabet
  return $ Hashtag $ pack t

retweet = do
  string "RT"
  return Retweet

plaintext = do
--  notFollowedBy link
  t <- many1 (noneOf whitespaces)

  return $ PlainText $ pack t

spcs = do
    s <- many1 (oneOf whitespaces)
    return $ Spaces $ pack s

hashOrText = try hashtag <|> plaintext

usernameOrText = try username <|> plaintext

linkOrText = try link <|> plaintext

chunk = try lb <|> username <|> try hashtag <|> try link <|> plaintext <|> spcs

tweet :: Parser [TweetElement]
tweet = do
    t <- many chunk
    return t


parseTweet :: Text -> [TweetElement]
parseTweet t = case parse tweet "tweet" t of
    Left err -> [Unparsable t] -- error $ (show err) ++ ("  >>>> " :: String) ++ (show t)
    Right ts -> ts
