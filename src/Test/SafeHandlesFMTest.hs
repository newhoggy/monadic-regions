{-# LANGUAGE ScopedTypeVariables #-}

-- Handle-based IO with the assured open/close protocol, see README
-- This file contains the tests for SafeHandlesFM.hs.
-- This is the implementation of F^RGN by Fluet and Morrisett (ICFP04),
-- adjusted for the application domain: IO and Handles rather than
-- memory allocation and pointers. The implementation relies on
-- term-level witnesses of region inclusion (subtyping).

module Test.SafeHandlesFMTest where

import Demo.SafeHandlesFM                        -- import the security kernel
import Control.Monad
import Control.Exception

-- single region test
test1 = runSIO $ do
    h1 <- newSHandle "fname1" ReadMode
    h2 <- newSHandle "fname1" ReadMode
    l1 <- shGetLine h1
    return True
    -- Can't do that: r escapes
    -- return h2

-- multiple region test
test2 = runSIO $ do
    h1 <- newSHandle "fname1" ReadMode
    h3 <- newRgn $ \ (SubRegion liftSIO) -> do
        h2 <- newSHandle "fname2" ReadMode
        h3 <- liftSIO $ newSHandle "fname3" ReadMode
        -- Can't allocate the handle outside the top region...
        -- h4 <- liftSIO $ liftSIO $ newSHandle "fname1" ReadMode
        l1 <- liftSIO $ shGetLine h1
        l1 <- shGetLine h2
        l1 <- liftSIO $ shGetLine h3
        return h3 -- but this is OK: h3 assigned to the parent region
        -- Can't do that: r escapes
        -- return h2
    l1 <- shGetLine h1
    l1 <- shGetLine h3
    return l1

-- An attempt to leak the computation. 
-- Now, it won't work...
{- 
test2' = runSIO (
    do
    h1 <- newSHandle "fname1" ReadMode
    let c1 = shGetLine h1
    c1
    ac <- newRgn (\(SubRegion liftSIO) ->
         do
         h2 <- newSHandle "fname2" ReadMode
         -- Fake the SIO type. Won't work though: h2 handle contaminates...
         return ((shGetLine h2) `asTypeOf` c1)
       )
    -- ac
    newRgn (\(SubRegion liftSIO) -> do
            -- That too is a type error: lack of polymorphism in newRgn
            -- ac
            return ()
           )

    return True
   )
-}

{- 
-- The above error is merely due to force monomorphism in the
-- monadic bind (do ac <- ...). One may think that a higher-rank type 
-- may give us a way around the monomorphic binding in do, and 
-- so to defeat the safety.
-- Fortunately, our approach prevents such a `way-around' and so
-- safety is preserved.

newtype WC = WC{unWC:: forall r . SIO r String}

test2'' = runSIO (
    do
    h1 <- newSHandle "/dev/null" ReadMode
    ac <- newRgn (\(SubRegion liftSIO) ->
         do
         h2 <- newSHandle "/dev/null" ReadMode
         -- Fake the SIO type. Won't work though... Good
         return (WC (shGetLine h2))
       )
    -- unWC ac
    newRgn (\(SubRegion liftSIO) -> do
            -- If this were allowed, safety would have been defeated.
            -- Fortunately, we can't even construct the WC value:
            -- the type error is reported at `return (WC (shGetLine h2))'
            -- above.
            unWC ac
            return ()
           )

    return True
   )

-}

-- Attempts to leak handles and computations via mutation
testref = runSIO $ do
    h1 <- newSHandle "fname1" ReadMode
    rh <- sNewIORef h1                        -- a ref cell holding a handle
    let c1 = shGetLine h1
    c1
    ra <- sNewIORef c1                        -- a ref cell holding a computation
    newRgn $ \ (SubRegion liftSIO) -> do
        h2 <- newSHandle "fname2" ReadMode
        sWriteIORef rh h1
        -- sWriteIORef rh h2 -- type error, 's' of the inner region escapes
        sWriteIORef ra (shGetLine h1) -- OK
        -- sWriteIORef ra (liftSIO (shGetLine h2)) -- error
        -- sWriteIORef ra (shGetLine h2) -- error: subtyping violation
        return ()
    newRgn $ \ (SubRegion liftSIO) -> do
        sReadIORef rh >>= liftSIO . shGetLine
        liftSIO (sReadIORef ra) >>= liftSIO . id
    return True


{- Ken's test:
A programming example using the enumerator (rather than cursor) pattern to
    (1) read a file name from a file
    (2) open that file and zip the two files' contents together
thus assuring that the files are accessed correctly and resources
disposed of completely.
-}


till condition iteration = loop where
  loop = do
      b <- condition
      if b then return () else iteration >> loop

test3 = runSIO $ do
    h1 <- newSHandle "/tmp/SafeHandles.hs" ReadMode
    h3 <- newRgn (test3_internal h1)
    -- once we closed h2, we write the rest of h1 into h3
    till (shIsEOF h1)
         (shGetLine h1 >>= shPutStrLn h3)
    shReport "test3 done"

-- The following shows that we do not have to put all IO code in
-- one big function. We can spread it out. The inferred type for the
-- following is _region-polymorphic_:
--   test3_internal :: SHandle (SIO t)
--                     -> SubRegion t s
--                     -> IORT s IO (SHandle (SIO t))

-- The code below requires many liftSIO applications. It is easy to
-- forget one, which would identify t and s in the inferred type above.
-- The error will be reported only in test3, a different function.
-- Along with the necessity to pass SubRegion as an extra argument,
-- these are the drawbacks of the term-level witnessing.

test3_internal h1 (SubRegion liftSIO) = do
  h2 <- newSHandle "/tmp/ex-file.conf" ReadMode
  fname <- shGetLine h2                -- read the fname from the config file
  -- allocate handle in the parent region
  h3 <- liftSIO (newSHandle fname WriteMode)
  -- zip h2 and h1 into h3
  liftSIO $ shPutStrLn h3 fname
  till (liftM2 (||) (shIsEOF h2) (liftSIO (shIsEOF h1)))
       (shGetLine h2 >>= liftSIO . shPutStrLn h3 >>
        liftSIO (shGetLine h1 >>= shPutStrLn h3))
  shReport "Finished zipping h1 and h2"
  return h3 -- but this is OK: h3 assigned to a parent region
  -- return h2 -- that would be an error: h2 can't escape



test4 h1 h2 = do
    d1 <- shGetLine h1
    shPutStrLn h2 d1
{-
Inferred type: NOT region-polymorphic:
*SafeHandlesFMTest> :t test4
test4 :: SHandle (SIO s) -> SHandle (SIO s) -> IORT s IO ()
-}

test41 h1 h2 (SubRegion liftSIO1) = do
    d1 <- liftSIO1 $ shGetLine h1
    shPutStrLn h2 d1
{-
Inferred type: somewhat region-polymorphic
*SafeHandlesFMTest> :t test41
test41 :: SHandle (SIO t) -> SHandle (SIO s) -> SubRegion t s -> IORT s IO ()
-}

test42 h1 h2 (SubRegion liftSIO1) = do
    d1 <- liftSIO1 $ shGetLine h1
    liftSIO1 $ shPutStrLn h2 d1
{-
Inferred type: still somewhat region-polymorphic
*SafeHandlesFMTest> :t test42
test42 :: SHandle (SIO t) -> SHandle (SIO t) -> SubRegion t t1 -> IORT t1 IO ()
-}

test43 h1 h2 (SubRegion liftSIO1) (SubRegion liftSIO2) = do
    d1 <- liftSIO1 $ shGetLine h1
    liftSIO2 $ shPutStrLn h2 d1
{-
Inferred type: now it is fully region-polymorphic. Two pieces
of evidence are required however:
*SafeHandlesFMTest> :t test43
test43 :: SHandle (SIO t)
          -> SHandle (SIO t2)
          -> SubRegion t t1
          -> SubRegion t2 t1
          -> IORT t1 IO ()
-}


-- Testing for problems in opening a file
-- We copy the contents of fname_in into fname_out.
-- If fname_in does not exist, write a message to fname_out to that effect.
-- Nothing bad happens if the file could not be opened as
-- no file reference (safe handle) is created in that case.

test_copy fname_in fname_out = do
    hout <- newSHandle fname_out WriteMode
    (do newRgn (\(SubRegion liftSIO) -> do
          hin <- newSHandle fname_in ReadMode
          till (shIsEOF hin)
               (shGetLine hin
                >>= liftSIO . shPutStrLn hout))
        shReport "Finished copying")
     `shCatch` \ (e :: SomeException) -> do
       shReport ("Exception caught: " ++ show e)
       shPutStrLn hout ("Copying failed: " ++ show e)

test_of1 = runSIO (test_copy "/etc/motd" "/tmp/t1")
test_of2 = runSIO (test_copy "/non-existent" "/tmp/t1")
