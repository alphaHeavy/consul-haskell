{-# LANGUAGE CPP #-}

module SocketUtils
  ( isPortOpen
  , simpleSockAddr
  ) where

import           Data.Word (Word8)
import           Foreign.C.Error (Errno(..), eCONNREFUSED)
import           GHC.IO.Exception (IOException(..))
import           Network.Socket (Socket, PortNumber, socket, connect, Family(AF_INET), SocketType(Stream), SockAddr(SockAddrInet), tupleToHostAddress)
import qualified Network.Socket as Socket
import           UnliftIO.Exception (try, bracket, throwIO)


-- | `socket` < 2.7.0.2 does not have `close'` which throws on error,
-- which we desire for sanity.
-- If it's not available, we fall back to the silently failing one.
close'fallback :: Socket -> IO ()
close'fallback =
  -- Unfortunately, `MIN_VERSION` does not accept a 4th argument,
  -- so we have to make the check for 2.8.0.
#if MIN_VERSION_network(2,8,0)
  Socket.close'
#else
  Socket.close
#endif


-- | Checks whether @connect()@ to a given TCPv4 `SockAddr` succeeds or
-- returns `eCONNREFUSED`.
--
-- Rethrows connection exceptions in all other cases (e.g. when the host
-- is unroutable).
isPortOpen :: SockAddr -> IO Bool
isPortOpen sockAddr = do
  bracket (socket AF_INET Stream 6 {- TCP -}) close'fallback $ \sock -> do
    res <- try $ connect sock sockAddr
    case res of
      Right () -> return True
      Left e ->
        if (Errno <$> ioe_errno e) == Just eCONNREFUSED
          then return False
          else throwIO e


-- | Creates a `SockAttr` from host IP and port number.
--
-- Example:
-- > simpleSockAddr (127,0,0,1) 8000
simpleSockAddr :: (Word8, Word8, Word8, Word8) -> PortNumber -> SockAddr
simpleSockAddr addr port = SockAddrInet port (tupleToHostAddress addr)
