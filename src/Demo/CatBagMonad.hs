{-# LANGUAGE Rank2Types #-}

module Demo.STMonad where

import Control.Applicative
import Control.Monad

-- Type trickery to keep the cat in the bag
data Bag s a = Bag a -- like ST 
data Cat s a = Cat a -- like STRef 

instance Monad (Bag s) where
    (>>=) = bindBag
    return = returnBag

instance Applicative (Bag s) where
    pure = return
    (Bag f) <*> (Bag s) = Bag (f s)

instance Functor (Bag s) where
    fmap f (Bag s) = Bag (f s)

runBag :: forall a. (forall s. Bag s a) -> a
runBag k = case k of Bag a -> a

newCat :: a -> (forall s. Bag s (Cat s a))
newCat x = Bag (Cat x)

readCat :: forall s. forall a. (Cat s a -> Bag s a)
readCat x = case x of (Cat v) -> Bag v

bindBag :: Bag s a -> (a -> Bag s b) -> Bag s b
bindBag s f = case s of Bag x -> f x

returnBag :: a -> forall s. Bag s a 
returnBag x = Bag x

-- Works, cat stays in the bag
test = runBag $ do
    cat <- newCat "meow"
    value <- readCat cat
    return value

-- --Doesn't compile - attempts to let the cat out
-- fail = runBag $ do
--     cat <- newCat "meow"
--     return cat