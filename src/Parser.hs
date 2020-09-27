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
  return $ r == "X"

priorityP = do
  _ <- char '('
  p <- alphaU 
  _ <- char ')'
  _ <- R.many $ char ' '
  return $ Just p

dateP = do
  s <- string "testDate"
  _ <- R.many $ char ' '
  return $ Just s

any_ = do
  c' <- satisfy (const True)
  return c'

anyBut p = do
  c' <- satisfy (\c -> not $ c `elem` p)
  return c'

pairP = do
  k <- R.many1 $ anyBut ": "
  _ <-   char ':'
  v <- R.many1 $ anyBut ": "
  _ <- R.many $ char ' '
  return (k,v)

projectP = do
  _  <- char '+'
  pr <- R.many1 $ anyBut " "
  _ <- R.many $ char ' '
  return $ Just pr

contextP = do
  _ <- char '@'
  cn <- R.many1 $ anyBut " "
  _ <- R.many $ char ' '
  return $ Just cn

descriptionP = do
  _ <- char '|'
  d <- R.many1 (anyBut "")
  _ <- char '|'
  _ <- R.many $ char ' '
  return d


taskP = do
  _                <- R.many $ char ' '
  done'            <- doneP
  priority'        <- option Nothing priorityP      
  description'     <- descriptionP
  project'         <- option Nothing projectP
  context'         <- option Nothing contextP
  pairs'           <- option [] (R.sepBy pairP (R.many1 $ char ' '))
  _                <- R.many $ char ' '
  return $ Task done'
                priority'
                description'
                project'
                context'
                (M.fromList pairs')

parseTask s = head [r | (r,[]) <- readP_to_S taskP s]

