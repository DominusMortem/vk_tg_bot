module MapUser where

import Control.Monad.State (State, get, modify, put, evalState, execState)
import qualified Data.Map as M
import Data.Text 

testData :: M.Map Int String
testData = M.empty

mapState :: (Int, String) -> State (M.Map Int String) ()
mapState (k, v) = do
  map <- get
  if (M.lookup k map) == Nothing
  then put $ M.insert k v map
  else put $ map

testMap a b = execState $ mapState (a, b)

mapState' :: (Int, String) -> State (M.Map Int String) ()
mapState' (k, v) = do
    dict <- get
    put (M.insert k v dict)

testMap' a b = execState $ mapState' (a, b)
