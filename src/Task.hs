module Task (Task (..)) where

import Data.Time

data Task
  = Task
    { done :: String
    , content :: String
    , due :: Maybe String
    } deriving (Eq , Show)
