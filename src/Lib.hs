module Lib
    ( someFunc
    , priorityP
    , f
    ) where

import Text.ParserCombinators.ReadP
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
    , project :: String
    , context :: String
    , pairs :: M.Map String String
  }


data Impl
  = Impl
    { fin :: Maybe Bool
    , pri :: Maybe Char
    , des :: String } deriving (Show)

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



implP = do
  x <- option Nothing finishedP
  p <- option Nothing priorityP
  -- d1 <- option Nothing dateP
  -- d2 <- option Nothing dateP
  d <- many1 $ alphaL <|> alphaU
  return $ Impl x p d


f = readP_to_S implP
