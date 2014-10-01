{-# LANGUAGE Rank2Types, KindSignatures, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

-- Handle-based IO with the assured open/close protocol, see README
-- This file contains the Security kernel. See SafeHandlesFMTest.hs for tests.
-- This is the implementation of F^RGN by Fluet and Morrisett (ICFP04),
-- adjusted for the application domain: IO and Handles rather than
-- memory allocation and pointers. The implementation relies on
-- term-level witnesses of region inclusion (subtyping).

module Demo.SafeHandlesFM 
    (IORT,			-- constructors not exported
     SIO,
     SHandle,
     SubRegion(..),

     runSIO,
     newRgn,
     newSHandle,
     IOMode(..),		-- re-exported from System.IO

     shGetLine,
     shPutStrLn,
     shIsEOF,

     shThrow,
     shCatch,
     shReport,

     sNewIORef,			-- IORef in SIO
     sReadIORef,
     sWriteIORef
     ) where

import System.IO
import Control.Applicative
import Control.Exception
import Control.Monad.Reader
import Data.IORef
import Prelude hiding (catch)


-- The IO monad with safe handles and regions (SIO) is implemented as 
-- the monad transformer IORT applied to IO. The monad transformer
-- seems overkill here; yet it prepares us for the final solution.
--
-- Each region maintains the state listing all open 
-- handles assigned to the region.
-- Since we already have IO, it is easy to implement the state as a 
-- mutable list (IORef of the list) and make this reference
-- a pervasive environment. 
-- We could have used implicit parameters or implicit configurations to
-- pass that IORef around. Here, we use ReaderT.

-- Since we do IO with our handles, we may be tempted to make
-- the IORT transformer an instance of MonadIO. However, that would 
-- give the user the ability to do any IO and so defeat the safety
-- guarantees. The purpose of IORT is to restrict permissible IO
-- operations to an assured set. Since we do need something like MonadIO,
-- we define our own private version here, RMonadIO. It is not exported.
-- Unlike MonadIO, our RMonadIO also supports catching and 
-- handling of exceptions.

-- A region is identified by a label, a type eigenvariable (see 's'
-- in ST s).

newtype IORT s m v = IORT{ unIORT:: ReaderT (IORef [Handle]) m v } 
    deriving (Monad, Applicative, Functor)

type SIO s = IORT s IO

-- A witness that the region labeled r is older than
-- (or, is the the parent of, the subtype of) the region labeled s
newtype SubRegion r s = SubRegion (forall v. SIO r v -> SIO s v)



-- RMonadIO is an internal class, a version of MonadIO
class Monad m => RMonadIO m where
    brace :: m a -> (a -> m b) -> (a -> m c) -> m c
    snag  :: Exception e => m a -> (e -> m a) -> m a
    lIO   :: IO a -> m a

instance RMonadIO IO where
    brace = bracket'
    snag  = catch'
    lIO   = id

-- The following makes sure that a low-level handle (System.IO.Handle)
-- cannot escape in an IO exception. Whenever an IO exception is caught,
-- we remove the handle from the exception before passing it to the
-- exception handler.
catch':: Exception e => IO a -> (e -> IO a) -> IO a
catch' m f = catch m (f . sanitizeExc)

bracket' :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket' before after m = bracket before after m
	      
sanitizeExc :: Exception e => e -> e
sanitizeExc e = e

instance RMonadIO m => RMonadIO (ReaderT r m) where
    brace before after during = ReaderT (\r ->
	let rr m = runReaderT m r
	in brace (rr before) (rr.after) (rr.during))
    snag m f = ReaderT(\r -> 
		 runReaderT m r `snag` \e -> runReaderT (f e) r)
    lIO = lift . lIO

instance RMonadIO m => RMonadIO (IORT s m) where
    brace before after during = IORT
	(brace (unIORT before) (unIORT.after) (unIORT.during))
    snag m f = IORT ( unIORT m `snag` (unIORT . f) )
    lIO = IORT . lIO

-- There is no explicit close operation. A handle is automatically
-- closed when its region is finished (normally or abnormally).

-- In Fluet, Morrisett this function is called `letRgn'
-- (See Fig 1 of their paper)
newRgn :: (forall s. SubRegion r s -> SIO s v) -> SIO r v
newRgn body = IORT (do
		    env_outer <- ask
		    -- Not just changing the label
		    let witness (IORT m) = lIO (runReaderT m env_outer)
		    lIO (runSIO (body (SubRegion witness))))


-- In Fluet, Morrisett this function is called `runRgn'
-- (See Fig 1 of their paper)
-- This function is exactly the same as that in SafeHandles1.hs
runSIO :: (forall s. SIO s v) -> IO v
runSIO m = brace (lIO (newIORef [])) after (runReaderT (unIORT m))
    where after handles = lIO (readIORef handles >>= mapM_ close)
          close h = do
	     hPutStrLn stderr ("Closing " ++ show h)
	     catch (hClose h) (\ (e :: SomeException) -> return ())

-- Our (safe) handle is labeled with the monad where it was created
-- The monad is the SIO monad with the region label s (type
-- eigenvariable). Eigenvariables are responsible for not letting the handles
-- escape from their assigned region. 

newtype SHandle (m :: * -> *) = SHandle Handle	-- data ctor not exported

-- Create a new safe handle and assign it to the current region 
-- We can apply the witness to the newSHandle operation to assign 
-- the safe handle to any parent region.
newSHandle :: FilePath -> IOMode -> SIO s (SHandle (SIO s))
newSHandle fname fmode = IORT r'
 where r' = do
	    h <- lIO $ openFile fname fmode -- may raise exc
	    handles <- ask
	    lIO $ modifyIORef handles (h:)
	    return (SHandle h)

-- Safe-handle-based IO...
-- The safe handle must be assigned to the current region.
-- We can use the witness to import the safe handle from any parent
-- region. The types _and_ the implementation are the same as 
-- those in SafeHandles1.hs

shGetLine :: SHandle (SIO s) -> SIO s String
shGetLine (SHandle h) = lIO (hGetLine h)

shPutStrLn :: SHandle (SIO s) -> String -> SIO s ()
shPutStrLn (SHandle h) = lIO . hPutStrLn h

shIsEOF :: SHandle (SIO s) -> SIO s Bool
shIsEOF (SHandle h) = lIO (hIsEOF h)

shThrow :: Exception e => e -> SIO s a
shThrow = lIO . throwIO

shCatch :: Exception e => SIO s a -> (e -> SIO s a) -> SIO s a
shCatch = snag

-- Useful for debugging
shReport :: String -> SIO s ()
shReport = lIO . hPutStrLn stderr

-- make IORef available with SIO, so we may write tests that attempt
-- to leak handles and computations with handles via assignment

sNewIORef :: a -> SIO s (IORef a)
sNewIORef = lIO . newIORef

sReadIORef :: IORef a -> SIO s a
sReadIORef = lIO . readIORef

sWriteIORef :: IORef a -> a -> SIO s ()
sWriteIORef r v = lIO $ writeIORef r v

