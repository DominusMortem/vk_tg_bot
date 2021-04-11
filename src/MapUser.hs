module MapUser where

import Control.Monad.State (State, get, put)
import qualified Data.Map  as M
import Data.Text 

mapSt :: (Int, String) -> State (M.Map Int String) ()
mapSt (k, v) = do
    dict <- get
    put (M.insert k v dict)
