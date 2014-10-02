module Demo.STMonadEx where

import Control.Monad.ST
import Data.STRef
import Control.Monad

-- sumST :: Num a => [a] -> a
invalidST xs = runST $ do           -- [1]
    n <- newSTRef 0             -- [2]
    forM_ xs $ \x -> do         -- [3]
        modifySTRef n (+x)      -- [4]
    readSTRef n                 -- [5]
    -- return n -- ERROR!
