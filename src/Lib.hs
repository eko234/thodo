module Lib
    ( runTaskP
    ) where

import Text.ParserCombinators.ReadP as R
import qualified Data.Map as M
import Control.Applicative

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
  return $ Just True

priorityP = do
  _ <- char '('
  p <- alphaU 
  _ <- char ')'
  return $ Just p

dateP = do
  s <- string "testDate"
  return $ Just s

number = do
  ns <- satisfy (\d -> d `elem` "1234567890")
  return ns

extra = do
  ex <- satisfy (\c -> c `elem` "-_|")
  return ex

pairP = do
  k <- many1 $ alphaL <|> alphaU <|> number <|> extra -- nums too
  _ <- char ':'
  v <- many1 $ alphaL <|> alphaU <|> number <|> extra
  return (k,v)

projectP = do
  _  <- char '+'
  pr <- many1 $ (alphaL <|> alphaU <|> number <|> extra)
  return $ Just pr

contextP = do
  _ <- char '@'
  cn <- many1 $ (alphaL <|> alphaU <|> number <|> extra)
  return $ Just cn

taskP = do
  done'            <- option Nothing finishedP      
  _                <- R.many1 $ char ' '
  priority'        <- option Nothing priorityP      
  _                <- R.many1 $ char ' '
  ceompletionDate' <- option Nothing dateP
  _                <- R.many1 $ char ' '
  creationDate'    <- option Nothing dateP
  _                <- R.many1 $ char ' '
  description'     <- R.many1 (alphaL <|> alphaU <|> number <|> extra)   
  _                <- R.many1 $ char ' '
  project'         <- option Nothing projectP
  _                <- R.many1 $ char ' '
  context'         <- option Nothing contextP
  _                <- R.many1 $ char ' '
  pairs'           <- option [] (R.sepBy pairP  (char ' '))
  _                <- R.many $ char ' '
  return $ Task done'
                priority'
                ceompletionDate'
                creationDate'
                description'
                project'
                context'
                (M.fromList pairs')

runTaskP = readP_to_S taskP
