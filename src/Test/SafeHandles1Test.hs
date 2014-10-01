-- Handle-based IO with the assured open/close protocol, see README
-- This file contains the tests for SafeHandles1.hs.
-- This is the warm-up example: all safe handles are in a single region.

module Test.SafeHandles1Test where

import Demo.SafeHandles1                        -- import the security kernel
import Control.Monad
import Control.Exception

-- single region test
test1 = runSIO (
    do
         h1 <- newSHandle "fname1" ReadMode
         h2 <- newSHandle "fname1" ReadMode
         l1 <- shGetLine h1
         return True
         -- Can't do that: r escapes
         -- return h2
        )

-- ``multiple'' region test
test2 = runSIO (
    do
    h1 <- newSHandle "fname1" ReadMode
    h3 <- (
         do
         h2 <- newSHandle "fname2" ReadMode
         h3 <- newSHandle "fname3" ReadMode
         l1 <- shGetLine h1
         l1 <- shGetLine h2
         l1 <- shGetLine h3
         return h3
        )
    l1 <- shGetLine h1
    l1 <- shGetLine h3
    return l1)

-- multiple naive regions. Handles can't be shared though...
test2' = runSIO (
    do
    h1 <- newSHandle "fname1" ReadMode
    l1 <- shGetLine h1
    let op = newSHandle "fname3" ReadMode
    res <- newNaiveReg (
         do
         h2 <- newSHandle "fname2" ReadMode
         h3 <- op -- things not involving regions can be shared
         -- Cannot use any handle from the parent region in the child region
         -- l1 <- shGetLine h1
         l2 <- shGetLine h2
         l3 <- shGetLine h3
         -- Cannot return any handle from a region that is opened
         -- in that region
         -- return h3
         return (l1 ++ l2 ++ l3)
        )
    h3 <- op
    l3 <- shGetLine h3
    return l1)

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

test3 = runSIO (do
  h1 <- newSHandle "/tmp/SafeHandles.hs" ReadMode
  h3 <- test3_internal h1
  -- once we closed h2, we write the rest of h1 into h3
  till (shIsEOF h1)
       (shGetLine h1 >>= shPutStrLn h3)
  shReport "test3 done"
  )

-- The following shows that we do not have to put all IO code in
-- one big function. We can spread it out.
test3_internal h1 = do
  h2 <- newSHandle "/tmp/ex-file.conf" ReadMode
  fname <- shGetLine h2                -- read the fname from the config file
  h3 <- newSHandle fname WriteMode
  -- zip h2 and h1 into h3
  shPutStrLn h3 fname
  till (liftM2 (||) (shIsEOF h2) (shIsEOF h1))
       (shGetLine h2 >>= shPutStrLn h3 >>
        shGetLine h1 >>= shPutStrLn h3)
  shReport "Finished zipping h1 and h2"
  return h3

test4 h1 h2 = do
              d1 <- shGetLine h1
              shPutStrLn h2 d1
{-
Inferred type: 
*SafeHandles1Test> :t test4
test4 :: SHandle (SIO s) -> SHandle (SIO s) -> IORT s IO ()
-}

