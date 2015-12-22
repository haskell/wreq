{-# LANGUAGE OverloadedStrings #-}

module Network.Wreq.Internal.Link
       ( links
       ) where

import Control.Applicative (many)
import Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString (ByteString)
import Network.Wreq.Types (Link(..))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

links :: B.ByteString -> [Link]
links hdr = case parseOnly f hdr of
              Left _   -> []
              Right xs -> xs
  where f = sepBy1 (link <* skipSpace) (char8 ',' *> skipSpace) <* endOfInput

link :: Parser Link
link = Link <$> url <*> many (char8 ';' *> skipSpace *> param)
  where url = char8 '<' *> A8.takeTill (=='>') <* char8 '>' <* skipSpace

param :: Parser (ByteString, ByteString)
param = do
  name <- paramName
  skipSpace *> "=" *> skipSpace
  c <- peekChar'
  let isTokenChar = A.inClass "!#$%&'()*+./0-9:<=>?@a-zA-Z[]^_`{|}~-"
  val <- case c of
           '"' -> quotedString
           _   -> A.takeWhile isTokenChar
  skipSpace
  return (name, val)

data Quot = Literal | Backslash

quotedString :: Parser ByteString
quotedString = char '"' *> (fixup <$> body) <* char '"'
  where body = A8.scan Literal $ \s c ->
          case (s,c) of
            (Literal,  '\\') -> backslash
            (Literal,  '"')  -> Nothing
            _                -> literal
        literal   = Just Literal
        backslash = Just Backslash
        fixup = B8.pack . go . B8.unpack
          where go ('\\' : x@'\\' : xs) = x : go xs
                go ('\\' : x@'"' : xs)  = x : go xs
                go (x : xs)             = x : go xs
                go xs                   = xs

paramName :: Parser ByteString
paramName = do
  name <- A.takeWhile1 $ A.inClass "a-zA-Z0-9!#$&+-.^_`|~"
  c <- peekChar
  return $ case c of
             Just '*' -> B8.snoc name '*'
             _        -> name
