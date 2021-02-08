module Parser
  ( parseTask
  ) where

import Text.ParserCombinators.ReadP as R
import qualified Data.Map as M
import Control.Applicative
import Data.Time
import Data.List
import Task

alphaU = satisfy (\c -> c `elem` ['A'..'Z'])
alphaL = satisfy (\c -> c `elem` ['a'..'z'])

doneP = do
  r <- between (char '[') (char ']') (R.many $ char 'X' <|> char ' ')
  _ <- R.many $ char ' '
  return r 

any_ = do
  c' <- satisfy (const True)
  return c'

dueP = do
  r <- between (char '<') (char '>') (R.many $ char 'N' <|> char '-')
  return $ Just r

contentP = do
  return "holi"


taskP = do
  _         <- R.many $ char ' '
  done'     <- doneP
  content'  <- contentP
  due'      <- option Nothing dueP
  _         <- R.many $ char ' '
  return $ Task done'
                content'
                due'

parseTask s = head [r | (r,[]) <- readP_to_S taskP s]

