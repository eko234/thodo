module Actions where

import Task 
import Data.Ord
import Data.Char

markAsComplete :: Task -> Task
markAsComplete t = t { done = True }

markAsIncomplete :: Task -> Task
markAsIncomplete t = t { done = False }

toggleComplete :: Task -> Task
toggleComplete t = t { done = not (done t) }

increasePriority :: Task -> Task
increasePriority t
  = case priority t of
    Nothing -> t { priority = Just 'Z' }
    Just pr -> if pr == 'A'
               then t
               else t { priority = Just $ chr $ 1 - (ord pr) }

decreasePriority :: Task -> Task
decreasePriority t
  = case priority t of
    Nothing -> t
    Just pr -> if pr == 'Z'
               then t
               else t { priority = Just $ chr $ 1 + (ord pr) }

setPriority :: Task -> Char -> Task
setPriority t c = t { priority = Just c }

deleteKV = 1
addKV = 2
modifyKV = 3
sortByPriority = 4
sortByKV = 5
getFromProject = 6
getFromContext = 7
listContexts = 8
listProjects = 9
