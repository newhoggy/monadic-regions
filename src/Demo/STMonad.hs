{-# LANGUAGE Rank2Types #-}

module Demo.STMonad where

import Control.Applicative
import Control.Monad

-- Type trickery to keep the STRef in the ST
-- `s` is a phantom type because it does not appear in the type constructor.
data ST s a = ST a -- like ST 
data STRef s a = STRef a -- like STRef 

instance Monad (ST s) where
    (>>=) = bindST
    return = returnST

instance Applicative (ST s) where
    pure = return
    (ST f) <*> (ST s) = ST (f s)

instance Functor (ST s) where
    fmap f (ST s) = ST (f s)

runST :: forall a. (forall s. ST s a) -> a
runST k = case k of ST a -> a

newSTRef :: a -> (forall s. ST s (STRef s a))
newSTRef x = ST (STRef x)

readSTRef :: forall s. forall a. (STRef s a -> ST s a)
readSTRef x = case x of (STRef v) -> ST v

bindST :: ST s a -> (a -> ST s b) -> ST s b
bindST s f = case s of ST x -> f x

returnST :: a -> forall s. ST s a 
returnST x = ST x

-- Works, STRef stays in the ST
test = runST $ do
    ref <- newSTRef "meow"
    value <- readSTRef ref
    return value

-- --Doesn't compile - attempts to let the STRef out
-- fail = runST $ do
--     ref <- newSTRef "meow"
--     return ref