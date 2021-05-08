module MapUser 
    ( mapSt
    ) where

import Control.Monad.State (State, get, put)
import qualified Data.Map   as M

mapSt :: (Int, Int) -> State (M.Map Int Int) ()
mapSt (k, v) = do
    dict <- get
    put (M.insert k v dict)
