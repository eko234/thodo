module Task (Task (..)) where

import Data.Map as M

data Task
  = Task
    { done :: Bool
    , priority :: Maybe Char
    , description :: String
    , project :: Maybe String
    , context :: Maybe String
    , pairs :: (M.Map String String)
    } deriving (Eq , Show)
