{-# LANGUAGE Rank2Types #-}

module Demo.STMonad where

import Control.Applicative
import Control.Monad

-- Type trickery to keep the cat in the ST
data ST s a = ST a -- like ST 
data Cat s a = Cat a -- like STRef 

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

newCat :: a -> (forall s. ST s (Cat s a))
newCat x = ST (Cat x)

readCat :: forall s. forall a. (Cat s a -> ST s a)
readCat x = case x of (Cat v) -> ST v

bindST :: ST s a -> (a -> ST s b) -> ST s b
bindST s f = case s of ST x -> f x

returnST :: a -> forall s. ST s a 
returnST x = ST x

-- Works, cat stays in the ST
test = runST $ do
    cat <- newCat "meow"
    value <- readCat cat
    return value

-- --Doesn't compile - attempts to let the cat out
-- fail = runST $ do
--     cat <- newCat "meow"
--     return cat