module Lib
    ( parseTask
    , Task
    ) where

import Text.ParserCombinators.ReadP as R
import qualified Data.Map as M
import Control.Applicative
import Data.Time
import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Task =
  Task
    { finished :: Maybe Bool
    , priority :: Maybe Char
    , startDate :: Maybe String -- TODO add a proper date format
    , endDate :: Maybe String
    , description :: String
    , project :: Maybe String
    , context :: Maybe String
    , pairs :: (M.Map String String)
  } deriving (Eq , Show)

alphaU = satisfy (\c -> c `elem` ['A'..'Z'])
alphaL = satisfy (\c -> c `elem` ['a'..'z'])


finishedP = do
  x <- char 'X'
  _ <- R.many $ char ' '
  return $ Just True

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

number = do
  ns <- satisfy (\d -> d `elem` "1234567890")
  return ns

extra = do
  ex <- satisfy (\c -> c `elem` "-_|")
  return ex

--
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
  done'            <- option Nothing finishedP      
  priority'        <- option Nothing priorityP      
  ceompletionDate' <- option Nothing dateP
  creationDate'    <- option Nothing dateP
  description'     <- descriptionP
  project'         <- option Nothing projectP
  context'         <- option Nothing contextP
  pairs'           <- option [] (R.sepBy pairP  (R.many1 $ char ' '))
  _                <- R.many $ char ' '
  return $ Task done'
                priority'
                ceompletionDate'
                creationDate'
                description'
                project'
                context'
                (M.fromList pairs')

parseTask s = head [r | (r,[]) <- readP_to_S taskP s]

