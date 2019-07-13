module SocketUtils
  ( isPortOpen
  , simpleSockAddr
  ) where

import           Data.Word (Word8)
import           Foreign.C.Error (Errno(..), eCONNREFUSED)
import           GHC.IO.Exception (IOException(..))
import           Network.Socket (PortNumber, socket, connect, close', Family(AF_INET), SocketType(Stream), SockAddr(SockAddrInet), tupleToHostAddress)
import           UnliftIO.Exception (try, bracket, throwIO)


-- | Checks whether @connect()@ to a given TCPv4 `SockAddr` succeeds or
-- returns `eCONNREFUSED`.
--
-- Rethrows connection exceptions in all other cases (e.g. when the host
-- is unroutable).
isPortOpen :: SockAddr -> IO Bool
isPortOpen sockAddr = do
  bracket (socket AF_INET Stream 6 {- TCP -}) close' $ \sock -> do
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
