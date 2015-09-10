{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- |
-- Utilities for parsing textual catalogs
module Celestial.Catalog where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T



newtype LineParser a = LineParser (StateT Text (Except String) a)
                       deriving (Functor,Applicative)

instance Monad LineParser where
  return = pure
  LineParser m >>= f = LineParser $ m >>= (\a -> case f a of LineParser n -> n)
  fail = LineParser . lift . throwE 

runLineParser :: LineParser a -> Text -> Either String a
runLineParser (LineParser m) str =
  case runExcept $ evalStateT m str of
    Left err -> Left $ err ++ " : `" ++ T.unpack str ++ "'"
    a        -> a

intField :: Int -> LineParser Int
intField n = readWith n $ T.decimal . T.dropWhile (==' ')

signField :: Num a => LineParser (a -> a)
signField = do
  s <- chunk 1
  case s of
    "-" -> return negate
    "+" -> return id
    " " -> return id
    _   -> fail "Bad sign"

skip :: Int -> LineParser ()
skip n = void $ chunk n

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

readWith :: Int -> T.Reader a -> LineParser a
readWith n reader = do
  s <- chunk n
  case reader s of
    Right (a,rest)
      | T.null rest -> return a
      | otherwise   -> fail "Field consumed incompletely"
    Left  err       -> fail $ err ++ " (" ++ T.unpack s ++ ")"

chunk :: Int -> LineParser Text
chunk n = LineParser $ do
  (ch,rest) <- T.splitAt n <$> get
  put rest
  return ch
    
