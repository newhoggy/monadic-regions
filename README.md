monadic-regions
===============

Implement Monadic Regions.

Manual Resource Management (common errors)
=================================
* Accessing an already closed resource
* Forgetting to deallocate a resource
* Untimely deallocation of a resource
* Double deallocation

pp 1 "Abstract"

Manual Resource Management (in Haskell)
=================================
* Runtime cost: Runtime check on access
* Garbage collection:
    - difficult to reason about timeliness
    - no guarantees when/if finalizers are run
    - bracket functions can leak invalid handles

pp 1 "Introduction"

Finalizers (System.Mem.Weak)
============================
mkWeak :: k -> v -> Maybe (IO ()) -> IO (Weak v)

The storage manager attempts to run the finalizer(s) for an object soon
after the object dies, but promptness is not guaranteed.

It is not guaranteed that a finalizer will eventually run, and no attempt
is made to run outstanding finalizers when the program exits. Therefore
finalizers should not be relied on to clean up resources

With Functions
==============
    withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

    unsafeWithFileCall =
        withFile "FilePath" ReadMode return >>= hGetLine

pp 4 "2.2 Implementation"

Objective (features)
====================
* Statically assured correctness
* Region polymorphism and Implicit region subtyping (??? Where mentioned?)
* No witness terms

pp 1 "Abstract"

Objective (preserved)
=====================
* Higher Order Functions
* Mutable State
* Recursion
* Runtime Exceptions
* Arbitrary: 
    - # resources           - resource types
    - deallocation order    - dynamic lifetimes

pp 1 "Abstract"

Solutions
=========
* Concept: Let Regions
* Implementations:
    - Safe IO in a Single region
    - Nested regions with explicit witness terms
    - Nested regions with added sharing
    - Nested regions using monad transformers
    - Nested regions with dynamic resource life times
    - Tracked Type State in a parameterized monad

pp 1-2 "Introduction"

Let Region
==========
* Nested region box diagram
* Challenges:
    - Statically scope resource lifetimes to a region
    - Maintain region polymorphism and subtyping
    - Keep notation convenient (no annotations or coercions)
    - Allowing unnested resource lifetimes
    - Allow dynamic resource lifetimes

Motivating example
==================
1. Open two files for reading, one of them a configuration file
2. Read the name of an output file from the configuration file
3. Open the output file and zipe the contents of both input files
   into the output file
4. Close the configuration file
5. Copy the rest, if any of the other input file to the output file

pp 2 "1.1 Motivating example"

Inspiration - ST Monad (State-Thread)
=====================================
* Allows use of mutable state in a referentially transparent function
* Prevents leaking of immutable state using the type system

pp 2 "Our contributions"

ST Monad Example
================
    sumST :: Num a => [a] -> a
    sumST xs = runST $ do           -- [1]
        n <- newSTRef 0             -- [2]
        forM_ xs $ \x -> do         -- [3]
            modifySTRef n (+x)      -- [4]
        readSTRef n                 -- [5]

ST Monad Example
================
1. `runST` takes out stateful code and makes it pure again.
2. Create an `STRef` (place in memory to store values)
3. For each element of `xs` ..
4. Add it to what we have in `n`.
5. Read the value of `n`, and return it.

ST Monad Invalid Example
========================
    invalidST xs = runST $ do
        n <- newSTRef 0
        forM_ xs $ \x -> do
            modifySTRef n (+x)
        return n -- ERROR!

ST Monad Invalid Example
========================
    Couldn't match expected type ‘a’ with actual type ‘STRef s a1’
      because type variable ‘s’ would escape its scope
    This (rigid, skolem) type variable is bound by
      a type expected by the context: ST s a
      at Demo/STMonadEx.hs:(8,12)-(12,12)
    Relevant bindings include
      n :: STRef s a1 (bound at Demo/STMonadEx.hs:9:5)
      xs :: [a1] (bound at Demo/STMonadEx.hs:8:7)
      sumST :: [a1] -> a (bound at Demo/STMonadEx.hs:8:1)
    In the first argument of ‘return’, namely ‘n’
    In a stmt of a 'do' block: return n

ST Monad - How does it work?
============================
* Phantom types
* Rank-2 Polymorphism

Phantom Types
=============
* `data Foo a v = Foo v`
* `a` is the phantom type because it doesn't appear in the
  type constructor
* Allows to encode compile time type information.

Rank-1 polymorphism
===================
* `map :: forall a b. (a -> b) -> [a] -> [b]`
* Introduces the type variables `a` and `b` an into scope

Rank-2 polymorphism
===================
* `foo :: (forall s. Foo s v) -> String v`
* `foo :: forall v. (forall s. Foo s v) -> String v`
* Introduces the type variable `v` into scope
* Introduces the type variable `s` into inner scope
* `s` cannot escape

ST Monad type interface
=======================
    data ST s a
    data STRef s a
    runST :: (forall s. ST s a) -> a
    newSTRef :: a -> ST s (STRef s a)
    modifySTRef :: STRef s a -> (a -> a) -> ST s ()

ST Monad explanation
====================
* Expressions containin `s` cannot escape the computation
* Values of type `STRef s a` persist until the end of computaton

Single Region Safe File IO (interface)
======================================
    type SIO s = ...
    newtype SHandle (m :: * -> *) = ...
    runSIO :: (forall s. SIO s v) -> IO v
    newSHandle :: FilePath -> IOMode -> SIO s (SHandle (SIO s))
    shGetLine :: SHandle (SIO s) -> SIO s String
    shPutStrLn :: SHandle (SIO s) -> String -> SIO s ()
    shIsEOF :: SHandle (SIO s) -> SIO s Bool
    shThrow :: Exception e => e -> SIO s a
    shCatch :: Exception e => SIO s a -> (e -> SIO s a) -> SIO s a
    shReport :: String -> SIO s ()

pp 3 "2.1 Interface"

Single Region Safe File IO (Comparison to Haskell IO)
=====================================================
    openFile :: FilePath -> IOMode -> IO Handle
    hGetLine :: Handle -> IO String
    hPutStrLn :: Handle -> String -> IO ()
    hClose :: Handle -> IO ()
    throw :: Exception e => e -> a
    catch :: Exception e => IO a -> (e -> IO a) -> IO a

Single Region Safe File IO (motivating example 1)
=================================================
    till condition iteration = loop where
      loop = do
        b <- condition
        if b then return () else iteration >> loop

    test3 = runSIO $ do
      h1 <- newSHandle "/tmp/SafeHandles.hs" ReadMode
      h3 <- test3_internal h1
      -- once we closed h2, we write the rest of h1 into h3
      till (shIsEOF h1)
           (shGetLine h1 >>= shPutStrLn h3)
      shReport "test3 done"

pp 3 "2.1 Interface"

Single Region Safe File IO (motivating example 2)
=================================================
    -- The following shows that we do not have to put all IO code in
    -- one big function. We can spread it out.
    test3_internal :: SHandle (SIO s) -> IORT s IO (SHandle (SIO s))
    test3_internal h1 = do
      h2 <- newSHandle "/tmp/ex-file.conf" ReadMode
      fname <- shGetLine h2                -- read the fname from the config file
      h3 <- newSHandle fname WriteMode
      -- zip h2 and h1 into h3
      shPutStrLn h3 fname
      -- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
      till (liftM2 (||) (shIsEOF h2) (shIsEOF h1))
           (shGetLine h2 >>= shPutStrLn h3 >>
            shGetLine h1 >>= shPutStrLn h3)
      shReport "Finished zipping h1 and h2"
      return h3

pp 3 "2.1 Interface"

Single Region Safe File IO (output)
===================================
    Finished zipping h1 and h2
    test 3 done
    Closing {handle: /tmp/t1}
    Closing {handle: /tmp/ex-file.conf}
    Closing {handle: /tmp/SafeHandles.hs}

pp 3 "2.1 Interface"

Single Region Safe File IO (implementation)
===========================================
    newtype IORT s m v =
        IORT{ unIORT:: ReaderT (IORef [Handle]) m v } 
        deriving (Applicative, Monad, Functor)

    type SIO s = IORT s IO

pp 3 "2.2 Implementation"

Single Region Safe File IO (implementation)
===========================================
* Relies on Standard Haskell IO
* Maintain list of created handles to close and end of computation.
* Internal handle type still Handle

pp 3 "2.2 Implementation"

Single Region Safe File IO (implementation)
===========================================
    -- RMonadIO is an internal class, a version of MonadIO
    class Monad m => RMonadIO m where
        brace :: m a -> (a -> m b) -> (a -> m c) -> m c
        snag  :: Exception e => m a -> (e -> m a) -> m a
        lIO   :: IO a -> m a

    instance RMonadIO IO where
        brace = ...
        snag  = ...
        lIO   = id

pp 4 "2.2 Implementation"

Single Region Safe File IO (implementation, Compare Haskell IO)
===============================================================
    class Monad m => MonadIO m where
        liftIO :: IO a -> m a

Single Region Safe File IO (implementation)
===========================================
    instance RMonadIO m => RMonadIO (ReaderT r m) where
        brace before after during = ReaderT (\r ->
            let rr m = runReaderT m r
            in brace (rr before) (rr.after) (rr.during))
        snag m f = ReaderT(\r -> 
                     runReaderT m r `snag` \e -> runReaderT (f e) r)
        lIO = lift . lIO

pp 4 "2.2 Implementation"

Single Region Safe File IO (implementation)
===========================================
    instance RMonadIO m => RMonadIO (IORT s m) where
        brace before after during = IORT
            (brace (unIORT before) (unIORT.after) (unIORT.during))
        snag m f = IORT ( unIORT m `snag` (unIORT . f) )
        lIO = IORT . lIO

pp 4 "2.2 Implementation"

Single Region Safe File IO (assessment)
=======================================
* Safe handle access.
* Zero-cost handle access.
* Deallocation statically assured.
* Handle allocation/deallocation overhead.
* Deallocation NOT timely!

pp 4 "2.2 Implementation"

Multi-region no sharing
=======================
    newNaiveReg :: (forall s. SIO s v) -> SIO s' v
    newNaiveReg m = lIO (runSIO m)

pp 5 "3.1 Implementation"

Multi-region no sharing (example)
=================================
    test2' = runSIO $ do
      h1 <- newSHandle "fname1" ReadMode
      l1 <- shGetLine h1
      let op = newSHandle "fname3" ReadMode
      res <- newNaiveReg $ do
        h2 <- newSHandle "fname2" ReadMode
        h3 <- op -- things not involving regions can be shared
        -- Cannot use any handle from the parent region in the child region
        -- l1 <- shGetLine h1 -- ERROR!
        l2 <- shGetLine h2
        l3 <- shGetLine h3
        -- Cannot return any handle from a region that is opened in that region
        -- return h3 -- ERROR!
        return (l1 ++ l2 ++ l3)
      h3 <- op
      l3 <- shGetLine h3
      return l1

pp 5 "3.1 Implementation"

Multi-region no sharing (motivating example)
============================================
✓ Want config file opened in child 
✓ Input file opened in parent region
✗ Input file accessed in child region
✓ Output file opened in child region
✗ Output file accessed in parent region

pp 5 "3.1 Implementation"

Multi-region, Launchbury and Sabry
==================================
    newLSRgn :: (forall s. SIO (r, s) v) -> SIO r v

    importSHandle :: SHandle (SIO r) -> SHandle (SIO (r, s))

Can only use parent handles from child, not the reverse.

pp 5 "3.2 Using a parent region from a child computation"

Multi-region, Fluett and Morriset
=================================
    -- A witness that the region labeled r is older than
    -- (or, is the the parent of, the subtype of) the region labeled s
    newtype SubRegion r s = SubRegion (forall v. SIO r v -> SIO s v)

    -- In Fluet, Morrisett this function is called `letRgn'
    -- (See Fig 1 of their paper)
    newRgn :: (forall s. SubRegion r s -> SIO s v) -> SIO r v

pp 5 "3.2 Using a parent region from a child computation"

Multi-region, Fluett and Morriset (motivating example)
======================================================
    test3 = runSIO $ do
        h1 <- newSHandle "/tmp/SafeHandles.hs" ReadMode
        h3 <- newRgn (test3_internal h1)
        -- once we closed h2, we write the rest of h1 into h3
        till (shIsEOF h1)
             (shGetLine h1 >>= shPutStrLn h3)
        shReport "test3 done"

pp 5 "3.2 Using a parent region from a child computation"

Multi-region, Fluett and Morriset (motivating example)
======================================================
    test3_internal :: SHandle (SIO t) -> SubRegion t s
                        -> IORT s IO (SHandle (SIO t)) -- Region polymorphic
    test3_internal h1 (SubRegion liftSIO) = do
      h2 <- newSHandle "/tmp/ex-file.conf" ReadMode
      fname <- shGetLine h2         -- read the fname from the config file
      h3 <- liftSIO (newSHandle fname WriteMode) -- allocate in parent
      -- zip h2 and h1 into h3
      liftSIO $ shPutStrLn h3 fname
      till (liftM2 (||) (shIsEOF h2) (liftSIO (shIsEOF h1)))
           (shGetLine h2 >>= liftSIO . shPutStrLn h3 >>
            liftSIO (shGetLine h1 >>= shPutStrLn h3))
      shReport "Finished zipping h1 and h2"
      return h3 -- but this is OK: h3 assigned to a parent region
      -- return h2 -- ERROR! h2 can't escape

pp 6 "3.2 Using a parent region from a child computation"

Multi-region, Fluett and Morriset (Output)
==============================================

pp 5 "3.2 Using a parent region from a child computation"

Multi-region, Fluett and Morriset (assessment)
==============================================
* Provides xxx
* Requires many `liftSIO` applications
* Requires extra `SubRegion` argument






