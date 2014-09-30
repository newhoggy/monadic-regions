{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-
  Handle-based IO with the statically assured open/close protocol:
  -- one may perform i/o only when the handle is opened
  -- once the handle is closed, it stays closed
  -- one cannot close the handle more than once
  -- at the end of the program, all handles are closed.

There is no requirement that the opening/closing of handles must be
properly nested. That is, it is not necessary to close handles in the
inverse order they have been opened. As shown in the last example
(suggested by Ken), we open one file (e.g., the configuration file) to
read the name of the main file, close the configuration file and
continue working with the second file. There are no limitation as to
how many handles may be opened or kept open concurrently.

This is the simplified version of the code in multi-handle-io.hs,
without regions. Everything is static, which means we can't form
recursive computations.


Alas, although this approach can work out in the absence of
exceptions, it crumbles when any exceptions arise, and the latter is
inevitable.  See the comments in multi-handle-io.hs for details.

But see SafeHandles.hs for a hybrid solution, which is still better
than System.IO.

Joint work with Chung-chieh Shan.

-}

module Demo.MultiHandle0 where

import System.IO
import Control.Monad (liftM2)
import Control.Exception (try, catch, throwIO, SomeException, Exception)

-- ======================================================================
-- Security kernel below


-- ----------------------------------------------------------------------
-- safe handle and the TSIO monad

-- The TSIO monad is a (ST s)-like monad with a state whose type
-- may vary from action to action (see class Monadish below). 
-- The state can informally be described as
--
-- kind State = (Counter, ActiveLabels)
-- kind Counter = Peano numeral for producing handle labels
-- kind ActiveLabels = [Label]  -- labels of the open handles
-- kind Label = Peano numeral
--
-- Since we don't have kind declarations in Haskell, we have to emulate
-- above using Peano numerals and heterogeneous lists built with the type
-- constant N and the binary type constructor C h t. This reminds one
-- of emulating data structures in Scheme/Lisp using only cons, car, cdr, nil
-- and pair?.

-- Handles can be opened and closed at will, in any order. 
-- A created handle is labeled with the new label, which is inserted in 
-- the active label list. The tshClose operation removes the label 
-- from the list.

-- TSIO is operationally indistinguishable from IO. All of its state
-- is phantom.
newtype TSIO s si so a = TSIO {unTSIO :: IO a}
tsio_in  :: TSIO s si so a -> si; tsio_in  = undefined
tsio_out :: TSIO s si so a -> so; tsio_out = undefined
tsio_s   :: TSIO s si so a -> s;  tsio_s   = undefined
tsio :: (StateOK si, StateOK so) => IO a -> s -> si -> so -> TSIO s si so a
tsio m _ _ _ = TSIO m

instance Monadish (TSIO s) where
  gret      = TSIO . return
  gbind m f = TSIO (unTSIO m >>= unTSIO . f)

instance Monad (TSIO s p p) where
  return = gret
  (>>=)  = gbind


-- lift into the TSIO monad
glift :: IO v -> TSIO s si si v
glift = TSIO


-- A handle is labeled by the eigenvariable and by the label.
-- The eigenvariable is opaque and merely prevents the escape of the handle.
-- The label lets us tell if the handle is active. It is far easier
-- to manipulate Peano numerals than eigenvariables (there is no
-- equality predicate on eigenvariables)
newtype TSHandle s l = TSHandle Handle
shandle :: Handle -> s -> l -> TSHandle s l
shandle h _ _ = TSHandle h
shandle_l :: TSHandle s l -> (s,l); shandle_l = undefined

-- Well-formedness predicate for the state, or, the description of
-- the kind state. Not exported.
-- It is used only internally, to add `static kinding' to the type-level
-- programs below
class StateOK s
-- Add: in any list of active labels, all labels are unique.
instance Nat0 counter => StateOK (counter, activeLabels)

-- Run the TSIO computation
-- In the resulting state, the list of active labels should be empty:
-- all handles must be closed. The resulting counter can be any, qc
runTSIO :: (forall s. TSIO s (Z,N) (qc,N) a) -> IO a
runTSIO m = unTSIO m  -- somehow, eta-expansion is needed here

-- low-level primitive to work with TSHandles. They are certainly not exported.
new_shandle ioh = r
 where
 r         = tsio (ioh >>= \h ->return (shandle h s cnt)) undefined undefined so
 s         = tsio_s r
 so        = (newcnt,newlst)
 newcnt    = inc cnt
 newlst    = C cnt lst
 (cnt,lst) = tsio_in r

-- Check that the handle is current. 
check_shandle sh@(TSHandle h) f = r
  where
  (s,l)  = shandle_l sh
  r  = tsio (f h) s undefined so
  so = tsio_in r
  (_,activelabels) = so
  -- If the label can be removed, it means it is present.
  _ = apply RemL (l,activelabels)


-- Remove the handle from the list of active. 
remove_shandle sh@(TSHandle h) f = r
  where
  (s,l)  = shandle_l sh
  r = tsio (f h) s undefined (cnt,activelabels')
  (cnt,activelabels) = tsio_in r
  activelabels' = apply RemL (l,activelabels)

test1 = runTSIO (glift (print "yes") +>>
	        (gret "yes again")) >>= print


-- All errors are considered fatal. See the paper
tshOpen fname mode = new_shandle (openFile fname mode)
tshClose sh = remove_shandle sh close
 where close h = do
		 hPutStrLn stderr $ "Closing " ++ show h
		 hClose h

-- regular handle operations
tshIsEOF sh    = check_shandle sh hIsEOF 
tshGetLine sh  = check_shandle sh hGetLine
tshPutStrLn sh = check_shandle sh . flip hPutStrLn

tshReport :: StateOK p => String -> TSIO s p p ()
tshReport str = TSIO (hPutStrLn stderr str)

onlyInSomeE :: IO (Either SomeException a) -> IO (Either SomeException a)
onlyInSomeE = id

-- The Final combinator
infixl 1 +>>>
(+>>>) :: TSIO s p q a -> TSIO s q r b -> TSIO s p r b
m1 +>>> m2 = TSIO (do
        r1 <- onlyInSomeE(try (unTSIO m1))
        r2 <- unTSIO m2 
        either throwIO (const (return r2)) r1)


test12 () = tshOpen "/etc/motd" ReadMode        >==
	    \h1 -> tshOpen "/dev/null" ReadMode >==
	    \h2 -> tshGetLine h1

-- Note the inferred type: two handles are open
-- Therefore, the following leads to a type error: non-closed handles left
-- test2r = runTSIO (test2 ())

-- The following is accepted (the constraint resolution is delayed due
-- to the polymorphic type, the error is reported below)
test13 () = tshOpen "/etc/motd" ReadMode >==
	    \h1 -> tshOpen "/dev/null" ReadMode >==
	    \h2 -> 
	       tshClose h1 +>>
	       tshClose h2 +>>
	       tshGetLine h1

-- The following is again a type error: although all the handles are closed,
-- we attempted to read from a closed handle (h1)
-- test3r = runTSIO (test3 ())

-- Here, the error is reported more immediately
{-
test13' = runTSIO (tshOpen "/etc/motd" ReadMode >==
	    \h1 -> tshOpen "/dev/null" ReadMode >==
	    \h2 -> 
	       tshClose h1 +>>
	       tshClose h2 +>>
	       tshGetLine h1)
-}

-- Here, we see non-nesting: h1 is opened before h2 and is closed before h2
test14 () = tshOpen "/etc/motd" ReadMode >==
	    \h1 -> tshOpen "/dev/null" ReadMode >==
	    \h2 -> 
	       tshClose h1 +>>
	       tshClose h2 +>>
	       gret "OK"
test14r = runTSIO (test14 ())

test15 () = tshOpen "/etc/motd" ReadMode >==
	    \h1 -> tshOpen "/dev/null" ReadMode >==
	    \h2 -> tshGetLine h1 >==
	      \l->
	       tshClose h2 +>>
	       -- tshClose h1 +>>	-- can't close the handle twice
	       tshClose h1 +>>
	       gret l
test15r = runTSIO (test15 ())

-- Standard tests from SafeHandlesTest.hs

test4 h1 h2 = do
	      d1 <- tshGetLine h1
	      tshPutStrLn h2 d1


{- Ken's test:
A programming example using the enumerator (rather than cursor) pattern to
    (1) read a file name from a file
    (2) open that file and zip the two files' contents together
thus assuring that the files are accessed correctly and resources
disposed of completely.
-}


till condition iteration = loop where
  loop = do b <- condition
            if b then return () else iteration >> loop

test3 = runTSIO (
  tshOpen "/tmp/SafeHandles.hs" ReadMode >== \h1 ->
  test3_internal h1 >== \h3 ->
  -- once we closed h2, we write the rest of h1 into h3
  till (tshIsEOF h1)
       (tshGetLine h1 >>= tshPutStrLn h3) >>
  tshReport "test3 done" +>>
  tshClose h1 +>>			-- if we omit it, error is reported
  tshClose h3				-- if we omit it, error is reported
  )

-- The following shows that we do not have to put all IO code in
-- one big function. We can spread it out. The inferred type for the
-- following is _region-polymorphic_.
test3_internal h1 = 
  tshOpen "/tmp/ex-file.conf" ReadMode >== \h2 ->
		-- read the fname from the config file
  tshGetLine h2 >== \fname ->
  tshOpen fname WriteMode >== \h3 ->
  -- zip h2 and h1 into h3
  tshPutStrLn h3 fname >>
  till (liftM2 (||) (tshIsEOF h2) (tshIsEOF h1))
       (tshGetLine h2 >>= tshPutStrLn h3 >>
        tshGetLine h1 >>= tshPutStrLn h3) >>
  tshReport "Finished zipping h1 and h2" +>>
  tshClose h2 +>>
  gret h3


-- The problem however remains if the second tshOpen throws an exception
-- Then h1 is not closed...
test_exc = runTSIO (
   tshOpen "/etc/motd" ReadMode >== \h1 ->
   tshGetLine h1 >== \l1 ->
   tshOpen "/etc/hosts" ReadMode >== \h2 ->  -- ReadMode is deliberate!
   tshPutStrLn h2 l1 +>>>		     -- must cause an error
   tshClose h1 +>>
   tshPutStrLn h2 l1 +>>>
   tshClose h2)


-- ----------------------------------------------------------------------
-- Parameterized, state-changing Monad-like

class Monadish m where
    gret :: a -> m p p a
    gbind :: m p q a -> (a -> m q r b) -> m p r b

-- Inject regular monads to be monadish things too
newtype MW m p q a = MW{ unMW:: m a }

instance Monad m => Monadish (MW m) where
    gret = MW . return
    gbind (MW m) f = MW (m >>= unMW . f)

-- some syntactic sugar

infixl 1 +>>
vm1 +>> vm2 = gbind vm1 (const vm2)

infixl 1 >==
m >== f = gbind m f



-- ----------------------------------------------------------------------
-- Peano numerals, used to label handles
-- We only require the successor and comparison operations

data Z
data S a
inc :: a -> S a
inc = undefined

class Nat0 n
instance Nat0 Z
instance Nat0 n => Nat0 (S n)

data HTrue
data HFalse

__ = __

-- n1 ==  n2 --> HTrue
-- otherwise --> HFalse
class EQN n1 n2 r | n1 n2 -> r
instance EQN Z Z HTrue
instance EQN Z (S n) HFalse
instance EQN (S n) Z HFalse
instance EQN n1 n2 r => EQN (S n1) (S n2) r

eqn :: EQN n1 n2 r => n1 -> n2 -> r; eqn = undefined

n0 :: Z = __
n1 = inc n0
n2 = inc n1
n3 = inc n2

test_eqn1 = eqn n0 n1
test_eqn2 = eqn n1 n1
test_eqn3 = eqn n2 n1

-- ----------------------------------------------------------------------
-- A bit of HList

data N     = N
data C a b = C a b

-- A heterogeneous apply operator

class Apply f a r | f a -> r where
  apply :: f -> a -> r
  apply = undefined			-- In case we use Apply for
                                        -- type-level computations only

-- Normal function application

instance Apply (x -> y) x y where
  apply f x = f x

-- Identity
data Id = Id

instance Apply Id x x where
  apply _ x = x

-- Type-level computation
-- remove a label from a list of labels

data Closure f arg			-- not exported

data RemL = RemL
instance (EQN l e bf, Apply (Closure RemL bf) (l,(C e r)) w) 
    => Apply RemL (l,(C e r)) w
instance Apply (Closure RemL HTrue) (l,(C l r)) r
instance Apply RemL (l,r) w => Apply (Closure RemL HFalse) (l,(C e r)) (C e w)

test_Reml1 = apply RemL (n0,(C n0 N))
-- Type error
-- test_Reml2 = apply RemL (n3,(C n0 (C n1 (C n2 N))))
test_Reml3 = apply RemL (n0,(C n0 (C n1 (C n2 N))))
test_Reml4 = apply RemL (n1,(C n0 (C n1 (C n2 N))))
test_Reml5 = apply RemL (n2,(C n0 (C n1 (C n2 N))))

-- Find an element in a heterogenous list satisfying a given predicate
-- The element must be present
newtype FindL f = FindL f
instance (Apply pred e bf, Apply (Closure (FindL pred) bf) (C e r) w)
    => Apply (FindL pred) (C e r) w
instance Apply (Closure (FindL pred) HTrue) (C e r) e
instance Apply (FindL pred) r w 
    => Apply (Closure (FindL pred) HFalse) (C e r) w

test_Findl1 = apply (FindL (EqtoN n1)) (C n0 (C n1 (C n2 N)))

newtype EqtoN n = EqtoN n
instance EQN n1 n2 r => Apply (EqtoN n1) n2 r

